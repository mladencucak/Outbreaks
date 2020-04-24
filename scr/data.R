########################################################
#Data
########################################################
list.of.packages <-
  c(
    "tidyverse",
    "readxl",
    "broom", 
    "data.table",
    "minpack.lm",
    "knitr",
    "zoo",
    "egg",
    "ggthemes",
    "here",
    "stringr",
    "lubridate"
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

if (length(new.packages))
  install.packages(new.packages, repos = c(CRAN="https://cran.r-project.org/"))


if ("reconPlots" %in% installed.packages() == FALSE) devtools::install_github("andrewheiss/reconPlots")

packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s", paste(list.of.packages[packages_load != TRUE]), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}

rm(packages_load, list.of.packages, new.packages)



###################################################################################
#Outbreaks
###################################################################################

#subset station data with stations present in data set

outbreaks <- read_csv(file  = here::here("dat", "outbreaks_fin.csv"))

outbreaks$date <- lubridate::mdy(outbreaks$date)

# outbreaks$dates <-as.Date(outbreaks$date,format="%m/%d/%Y") 
str(outbreaks)

outbreaks$jday <- as.Date(outbreaks$date,format="%m/%d/%Y") %>% yday()


cdfdt <- 
  outbreaks[outbreaks$Location!= "Oak Park",] %>%
  mutate(mon = month(date),
         day = day(date)) %>%
  group_by(mon, day) %>%
  unite(., date, mon, day, sep = "-", remove = FALSE) %>%
  mutate(d = as.Date(date, format = "%m-%d")) %>%
  ungroup() %>% 
  group_by(yr,mon, day) %>%
  mutate(count = n()) %>% 
  group_by(yr) %>%
  
  mutate(cums = cumsum(count)) %>% 
  dplyr::select(.,yr, jday, cums) %>% 
  ungroup() %>% 
  mutate(yr = as.factor(yr-min(yr-1)))

str(cdfdt)


ggplot(cdfdt) +
  geom_line(aes(jday, cums))+
  facet_wrap(~ yr, ncol = 3)+
  labs(y = "Cumulative sum of outbreaks",
       x = "Date") +
  theme_article()
ggsave(here::here("out", "fig", "Cumsum of outbreaks per year.png"),
       width = 6,
       height = 4.5,
       dpi = 620)



library(nlme)


start.vals <- c(ult = 1, omega = 1.4, theta = 45)
cdf.fun <- 
  function( jday,ult, omega, theta ){  ult*(1 - exp(-(jday/theta)^omega))}

cdf.fun(cdfdt$jday, 1, 1.4, 45)


w1 <- 
  nlme(cums ~ cdf.fun(jday,ult, omega, theta),
       fixed = list(ult~1, omega~1, theta ~ 1),
       random = ult ~ 1 | yr,
       # weights = varPower(fixed=.5),
       data=cdfdt,
       start = start.vals)



# library("optimx")
fit <- bbmle::mle2(cums ~ dnorm(mean = beta/eta * ((wet_dur - 3)/eta)^(beta - 1) *exp(((wet_dur - 3)/eta)^beta),
                                sd = sigma),
                   parameters = list(beta ~ temp * inoculum_dose,
                                     eta ~ temp * inoculum_dose),
                   start = list(beta = .001, eta = 1, sigma = 1), ## you have to be very careful about starting values
                   optimizer = "nlm", ## you can also play with different optimizers; or use contrained optimization or a reparameterization (e.g. for eta > 0)
                   data = dis_df)



