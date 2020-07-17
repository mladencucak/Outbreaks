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
    "lubridate"
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


disdf <-  read_csv(here::here("dat", "reports_summary.csv"))
wthcat <-  read_csv(here::here("out", "weather_categories.csv"))

df <- 
left_join(disdf, wthcat, by = "yr") %>% 
  dplyr::select(-c("yr","initrep","lastrep","inidoy","lastdoy"))

library("PerformanceAnalytics")
left_join(disdf, wthcat, by = "yr") %>% 
  dplyr::select(-c("yr","initrep","lastrep")) %>% 
PerformanceAnalytics::chart.Correlation(.)


full.model <- lm(sumrep ~ ., data = df)
summary(full.model)

library(tidyverse)
library(caret)
library(leaps)
library(MASS)
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(sumrep ~ ., data = df,
                    method = "lmStepAIC", 
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)

mod <- lm(sumrep ~ proprain_foi +sumrain_foi+proprain_ooi+propfrost_ooi+hellman+proprain_gs+sumrain_gs+sol_rad+favhours
                 , data = df)
summary(mod)


