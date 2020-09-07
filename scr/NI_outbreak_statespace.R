########################################################
# Libraries
########################################################
list.of.packages <-
  c(
    "tidyverse",
    "readxl",
    "data.table",
    "knitr",
    "zoo",
    "egg",
    "ggthemes",
    "here",
    "stringr",
    "lubridate",
    "coda",
    "bayesplot",
    "MCMCvis",
    "runjags",
    "gridExtra",
    "caret",
    "leaps",
    "MASS",
    "PerformanceAnalytics"
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

if (length(new.packages))
  install.packages(new.packages, repos = c(CRAN="https://cran.r-project.org/"))


packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s", paste(list.of.packages[packages_load != TRUE]), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}

rm(packages_load, list.of.packages, new.packages)


##############################################################################
# Analysis
##############################################################################

####
###
###Test!!!!!!!!!!!!!!!!!!!
# The correlation of number of favaourable hours does not necessarily mean how bad was the season mut these periods of bad weather
# Try to test if rolling means improve the prediction !!!!! 



disdf <-  read_csv(here::here("dat", "reports_summary.csv"))
wthcat <-  read_csv(here::here("out", "weather_categories.csv"))
load(file = here::here("out", "Conditions_pre&during.RData")) #conditions prior and during the outbrela report period
head(ddd)

disdf <- 
ddd %>% 
dplyr::select(-c("initrep","lastrep","inidoy","lastdoy", "sumrep")) %>% 
left_join(disdf,.,by = "yr") 
  

dta <- 
  left_join(disdf, wthcat, by = "yr") %>% 
  dplyr::select(-c("yr","initrep","lastrep","inidoy","lastdoy"))


#Check correlation matrix between all variables
left_join(disdf, wthcat, by = "yr") %>% 
  dplyr::select(-c("yr","initrep","lastrep")) %>% 
  PerformanceAnalytics::chart.Correlation(.)


# Fit linear regression
full.model <- lm(sumrep ~ ., data = dta)
summary(full.model)

step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

# Set seed for reproducibility+
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(sumrep ~ ., data = dta,
                    method = "lmStepAIC", 
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)



##############################################################################
# State space model
##############################################################################

#Load the data 

dta <- 
  left_join(disdf, wthcat, by = "yr") %>% 
  dplyr::select(-c("initrep","lastrep"))

# dta[ 12, 2:3] <-NA

dta %>%
  ggplot(aes(x = yr, y = sumrep)) +
  theme_bw() +
  geom_point() +
  geom_line(alpha = .4) +
  scale_x_continuous(breaks = 2003:2014,
                     labels = paste(2003:2014))

# dta <- rbind(dta, NA, NA, NA)
# dta$yr <- 2003:2017
# 
# dta$full_n <- dta$n
# 
# dta[ 10:12, "n"] <-NA 

#-----------------------------------------
#Run state space model without covarieties
#To run this script JAGS must be installed 
#install JAGS http://mcmc-jags.sourceforge.net

## JAGS model
model <- "model.txt"
jagsscript <- cat("
  model {
    ## Initialising
    mu[1] <- Y[1]

    ## Likelihood
    for(t in 2:N) {
      Y[t] ~ dnegbin(p[t], theta)
      Y_pred[t] ~ dnegbin(p[t], theta)
      p[t] <- theta/(theta + mu[t])
        log(mu[t]) <- phi[1] + phi[2] * Y[t-1]
    }
    
    ## Priors
    for(i in 1:2) {
      phi[i] ~ dnorm(0, 0.001)
    }
    beta ~ dnorm(0, 0.001)
    theta ~ dgamma(0.001, 0.001)
  }
", file = model)

# initialization
initfunction <- function(chain) {
  return(switch(chain,
                "1" = list(
                  .RNG.name="base::Super-Duper",
                  .RNG.seed=1),
                "2" = list(
                  .RNG.name="base::Super-Duper",
                  .RNG.seed=2),
                "3" = list(
                  .RNG.name="base::Super-Duper",
                  .RNG.seed=3),
  ))
}


# JAGS data
jags_data <- list("Y" = dta$sumrep,
                  "N" = length(dta$sumrep))

# parameters to be monitored
jags_params <- c("phi","theta","mu","Y_pred")

# run parallel JAGS
nChains <- 3
nAdaptSteps <- 2000
nBurninSteps <- 5000
nThinSteps <- 20
nUseSteps <- 6000
runJagsOut <- run.jags(method = "parallel",
                       model = model,
                       monitor = jags_params,
                       data = jags_data,
                       n.chains = nChains,
                       adapt = nAdaptSteps,
                       burnin = nBurninSteps,
                       sample = ceiling(nUseSteps/nChains),
                       thin = nThinSteps,
                       summarise = FALSE,
                       plots = FALSE,
                       inits = initfunction)
# coda samples - MCMC
coda_samples <- as.mcmc.list(runJagsOut)

# parameter estimates
estimates <- MCMCsummary(coda_samples, round = 4)
estimates

# predictions
Y_pred <- MCMCsummary(coda_samples, params = "Y_pred")



dta %>%
  dplyr::select(c("yr", "sumrep")) %>%
  mutate(
    pred = c(NA, Y_pred$mean),
    lower = c(NA, Y_pred$`2.5%`),
    upper = c(NA, Y_pred$`97.5%`)
  ) %>%
  ggplot(aes(x = yr, y = sumrep)) +
  geom_point(aes(x = yr, y = sumrep), color = "red") +
  theme_bw() +
  geom_point() +
  geom_line(alpha = .4) +
  geom_line(aes(y = pred), col = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = .2,
              fill = 4) +
  scale_x_continuous(breaks = 2003:2017,
                     labels = paste(2003:2017))+
ggsave(here::here("out", "base_model_diag", "base_model.png"))


MCMCsummary(coda_samples, c("phi", "theta"), round = 4) %>%
  bind_rows %>% write.csv(here::here("out", "base_model_diag", "diag.csv"))


# parameter estimates
posterior <- as.matrix(coda_samples[, c(1:3)])

p <- 
grid.arrange(mcmc_areas(posterior[, c(1,1)], prob = .95),
             mcmc_areas(posterior[, c(2,2)], prob = .95),
             mcmc_areas(posterior[, c(3,3)], prob = .95)+
               scale_x_continuous(limits = c(0,40)), 
             nrow = 1)

  ggsave(here::here("out", "base_model_diag", "params.png"), plot = p ,
         width = 7, height = 2)

mcmc_areas(posterior[, c(3,3)], prob = .95)+
  scale_x_continuous(limits = c(0,50))
p <- 
grid.arrange(mcmc_areas(posterior[, c(1,1)], prob = .95),
             mcmc_trace(posterior[, c(1,1)], prob = .95),
             mcmc_areas(posterior[, c(2,2)], prob = .95),
             mcmc_trace(posterior[, c(2,2)], prob = .95),
             mcmc_areas(posterior[, c(3,3)], prob = .95)+
               scale_x_continuous(limits = c(0,40)),
             mcmc_trace(posterior[, c(3,3)], prob = .95),
             ncol= 2,
             widths = c(1,2))

ggsave(here::here("out", "base_model_diag", "traceplots.png"),
         p, width = 8, height = 5)
shell.exec(here::here("out", "base_model_diag", "traceplots.png"))

autocorr.plot(coda_samples[[1]][, c(1:3)])
autocorr.plot(coda_samples[[2]][, c(1:3)])
autocorr.plot(coda_samples[[3]][, c(1:3)])

rm(p, posterior)


###########################################################
#Modeling the number of reports with covariate
############################################################
mods_ls <- list()

cov_df <- 
dta %>% 
  dplyr::select(-c("yr", "sumrep" ))



# The last day of report from the previous year needs to be used  
cov_df$lastdoy <- 
  c(NA, head(cov_df$lastdoy, -1))

#Redefine the model to add covarieties and monitor additional parameter
model <- "model.txt"
jagsscript <- cat("
  model {
    ## Initialising
    mu[1] <- Y[1]

    ## Likelihood
    for(t in 2:N) {
      Y[t] ~ dnegbin(p[t], theta)
      Y_pred[t] ~ dnegbin(p[t], theta)
      p[t] <- theta/(theta + mu[t])
      log(mu[t]) <- phi[1] + phi[2] * Y[t-1] + beta* cov[t]
    }
    
    ## Priors
    for(i in 1:2) {
      phi[i] ~ dnorm(0, 0.001)
    }
    beta ~ dnorm(0, 0.001)
    theta ~ dgamma(0.001, 0.001)
  }
", file = model)

#Loop over all covarieties and fit them separately 
names(cov_df)
for(i in seq(colnames(cov_df))) {
  # i = 2
  cov_name <- colnames(cov_df[, i])
  covariate <-  cov_df[, i] %>% pull()
  
  
  
  
  
  # JAGS data
  jags_data <- list("Y" = dta$sumrep,
                    "N" = length(dta$sumrep),
                    "cov" = covariate
  )
  
  # parameters to be monitored (beta added)
  jags_params <- c("phi","beta", "theta","mu", "Y_pred")
  
  
  runJagsOut <- run.jags(method = "parallel",
                         model = model,
                         monitor = jags_params,
                         data = jags_data,
                         n.chains = nChains,
                         adapt = nAdaptSteps,
                         burnin = nBurninSteps,
                         sample = ceiling(nUseSteps/nChains),
                         thin = nThinSteps,
                         summarise = FALSE,
                         plots = FALSE,
                         inits = initfunction)
  
  mods_ls[[i]] <- runJagsOut
  
  names(mods_ls)[i] <- cov_name
}

#Diagnostics
posterior_ls <- list()
pls <- list()


for(i in seq(names(mods_ls))) {
  
  runJagsOut <- mods_ls[[i]]
  cov_name <- names(mods_ls)[i]
  
  # coda samples - MCMC
  coda_samples <- as.mcmc.list(runJagsOut)
  
  # parameter estimates
  posterior <- as.matrix(coda_samples[,c(1:3)])
  posterior_ls[[i]] <- 
  bind_cols(rownames( MCMCsummary(coda_samples, c("phi","beta", "theta"), round = 4)) %>% tbl_df(),
            MCMCsummary(coda_samples, c("phi","beta", "theta"), round = 4))  
  
  posterior_ls[[i]] <-
  add_column(posterior_ls[[i]], cov = cov_name, .before = "value") %>% 
    rename(par = value)
  
  # predictions
  Y_pred <- MCMCsummary(coda_samples, params = "Y_pred")
  
  fundf <-
    dta %>%
    dplyr::select(c("yr", "sumrep",cov_name)) %>%
    mutate(pred = c(NA, Y_pred$mean),
           lower = c(NA, Y_pred$`2.5%`),
           upper = c(NA, Y_pred$`97.5%`))

  fundf %>%
    ggplot(aes(x = yr, y = sumrep)) +
    geom_point(aes(x = yr, y = sumrep), color = "red")+
    theme_bw() +
    geom_point() +
    geom_line(alpha = .4) +
    geom_line(aes(y = pred), col = "blue") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2, fill = 4) +
    scale_x_continuous(breaks = 2003:2017,
                       labels = paste(2003:2017))+
    ggsave(here::here("out", "model_diag",paste0(cov_name, ".png")))
  
  pls [[i]]<- 
  mcmc_areas(posterior[, c(3,3)], prob = .95)+
    ggplot2::labs(subtitle =  cov_name)
  
  pl <- 
  grid.arrange(mcmc_areas(posterior[, c(1,1)], prob = .95),
               mcmc_areas(posterior[, c(2,2)], prob = .95),
               mcmc_areas(posterior[, c(3,3)], prob = .95), 
               ncol = 3) 
    ggsave(
      here::here("out", "model_diag", paste0("pars", cov_name, ".png")),
      plot = pl,
      width = 4,
      height =2
    )
    rm(pl)
  
  # runJagsOut.mcmc <- as.mcmc(runJagsOut)
  # summary(runJagsOut.mcmc)
  # autocorr.plot(runJagsOut.mcmc)
  
}


grid.arrange(grobs = pls,
                  ncol  = 3) %>%
  
  ggsave(
    here::here("out", "model_diag", "betas.png"),
    .,
    width = 6,
    height = 6
  )
shell.exec(here::here("out", "model_diag", "betas.png"))



######################################################
#Modeling the day of the year when the initial was recorded 
#####################################################

init_mods_ls <- list()

cov_df <- 
  dta %>% 
  dplyr::select(-c("yr", "inidoy","inidoy", "sumrep","proprain_gs", "sumrain_gs",
                   "cond_during", "cond_till_lastrep","durrep",
                    "lastdoy",
                   "sol_rad", "favhours"))


# cov_df <- 
#   filter(dta,row_number()!=1)

# The last day of report from the previous year was modeled as predictor  
cov_df$lastdoy <- 
c(NA,head(cov_df$lastdoy, -1) )

for(i in seq(colnames(cov_df))) {
  # i = 1
  cov_name <- colnames(cov_df[, i])
  covariate <-  cov_df[, i] %>% pull()
  
  
  # JAGS data
  jags_data <- list("Y" = dta$inidoy,
                    "N" = length(dta$inidoy),
                    "cov" = covariate
  )
  
  runJagsOut <- run.jags(method = "parallel",
                         model = model,
                         monitor = jags_params,
                         data = jags_data,
                         n.chains = nChains,
                         adapt = nAdaptSteps,
                         burnin = nBurninSteps,
                         sample = ceiling(nUseSteps/nChains),
                         thin = nThinSteps,
                         summarise = FALSE,
                         plots = FALSE,
                         inits = initfunction)
  
  init_mods_ls[[i]] <- runJagsOut
  
  names(init_mods_ls)[i] <- cov_name
}

#Diagnostics
posterior_ls <- list()
pls <- list()


for(i in seq(names(init_mods_ls))) {
  
  runJagsOut <- init_mods_ls[[i]]
  cov_name <- names(init_mods_ls)[i]
  
  # coda samples - MCMC
  coda_samples <- as.mcmc.list(runJagsOut)
  
  # parameter estimates
  posterior <- as.matrix(coda_samples[,c(1:3)])
  posterior_ls[[i]] <- 
    
    bind_cols(rownames( MCMCsummary(coda_samples, c("phi","beta", "theta"), round = 4)) %>% tbl_df(),
              MCMCsummary(coda_samples, c("phi","beta", "theta"), round = 4))  
  
  posterior_ls[[i]] <-
    add_column(posterior_ls[[i]], cov = cov_name, .before = "value") %>% 
    rename(par = value)
  
  # predictions
  Y_pred <- MCMCsummary(coda_samples, params = "Y_pred")
  
  fundf <-
    dta %>%
    dplyr::select(c("yr", "sumrep",cov_name)) %>%
    mutate(pred = c(NA, Y_pred$mean),
           lower = c(NA, Y_pred$`2.5%`),
           upper = c(NA, Y_pred$`97.5%`))
  
  fundf %>%
    ggplot(aes(x = yr, y = sumrep)) +
    geom_point(aes(x = yr, y = sumrep), color = "red")+
    theme_bw() +
    geom_point() +
    geom_line(alpha = .4) +
    geom_line(aes(y = pred), col = "blue") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2, fill = 4) +
    scale_x_continuous(breaks = 2003:2017,
                       labels = paste(2003:2017))+
    ggsave(here::here("out", "model_diag_init",paste0(cov_name, ".png")))
  
  pls [[i]]<- 
    mcmc_areas(posterior[, c(3,3)], prob = .95)+
    ggplot2::labs(subtitle =  cov_name)
  
 
    grid.arrange(mcmc_areas(posterior[, c(1,1)], prob = .95),
                 mcmc_areas(posterior[, c(2,2)], prob = .95),
                 mcmc_areas(posterior[, c(3,3)], prob = .95),
                 ncol = 1) %>% 
  
  ggsave(
    here::here("out", "model_diag_init", paste0("pars", cov_name, ".png")),
    plot = .,
    width = 4,
    height =2
  )

  # runJagsOut.mcmc <- as.mcmc(runJagsOut)
  # summary(runJagsOut.mcmc)
  # autocorr.plot(runJagsOut.mcmc)
}



grid.arrange(grobs = pls,
                  ncol  = 3) %>%
  
  ggsave(
    here::here("out", "model_diag_init", "betas.png"),
    .,
    width = 6,
    height = 6
  )
shell.exec(here::here("out", "model_diag_init", "betas.png"))





