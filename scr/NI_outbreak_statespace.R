library(tidyverse)
library(coda)
library(bayesplot)
library(MCMCvis)
library(runjags)
library(gridExtra)

blightdat<-read.csv(here::here("tmp" , "PLB Outbreaks no oakpark.csv") , head=TRUE)

n_days <- 
  blightdat %>%
  group_by(yr) %>%
  summarise(n = n()) %>% 
  mutate(cov = rnorm(12))

n_days[ 12, 2:3] <-NA

n_days %>%
  ggplot(aes(x = yr, y = n)) +
  theme_bw() +
  geom_point() +
  geom_line(alpha = .4) +
  scale_x_continuous(breaks = 2003:2014,
                     labels = paste(2003:2014))

# n_days <- rbind(n_days, NA, NA, NA)
# n_days$yr <- 2003:2017
# 
# n_days$full_n <- n_days$n
# 
# n_days[ 10:12, "n"] <-NA 

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
      log(mu[t]) <- phi[1] + phi[2] * Y[t-1] + beta* cov[t-1]
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
jags_data <- list("Y" = n_days$n,
                  "N" = nrow(n_days),
                  "cov" = n_days$cov
                  )

# parameters to be monitored
jags_params <- c("phi","beta", "theta","mu", "Y_pred")

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

n_days <- n_days %>%
  mutate(pred = c(NA, Y_pred$mean),
         lower = c(NA, Y_pred$`2.5%`),
         upper = c(NA, Y_pred$`97.5%`))

n_days %>%
  ggplot(aes(x = yr, y = n)) +
  geom_point(aes(x = yr, y = full_n), color = "red")+
  theme_bw() +
  geom_point() +
  geom_line(alpha = .4) +
  geom_line(aes(y = pred), col = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2, fill = 4) +
  scale_x_continuous(breaks = 2003:2017,
                     labels = paste(2003:2017))

# diagnostics
posterior <- as.matrix(coda_samples[,c(1:3)])
mcmc_intervals(posterior)

grid.arrange(mcmc_areas(posterior[, c(1,1)], prob = .95),
             mcmc_areas(posterior[, c(2,2)], prob = .95),
             mcmc_areas(posterior[, c(3,3)], prob = .95))

grid.arrange(mcmc_trace(posterior[, c(1,1)], prob = .95),
             mcmc_trace(posterior[, c(2,2)], prob = .95),
             mcmc_trace(posterior[, c(3,3)], prob = .95))

autocorr.plot(coda_samples[[1]][, c(1:3)])
autocorr.plot(coda_samples[[2]][, c(1:3)])
autocorr.plot(coda_samples[[3]][, c(1:3)])
