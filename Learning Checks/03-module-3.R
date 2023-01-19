# Load Data and Dependencies ----------------------------------------------

library(lme4)
library(tidyverse)
# library(dplyr) # for data processing
library(lmtest) # for cluster-robust standard errors
library(sandwich) # for cluster-robust standard errors

data <- read.csv('heck2011.csv', fileEncoding = "UTF-8-BOM")

# Cluster-Robust Standard Errors ------------------------------------------

model <- lm(math ~ ses + female, data = data)
summary(model)

# ?sandwich::vcovCL
# ?lmtest::coeftest
model_crse <- coeftest(model, vcov = vcovCL, cluster = ~ schcode)
model_crse

# Estimation and Optimizers
data %>% 
  filter(schcode <= 10) %>% # subset data to make it easier to see
  ggplot(mapping = aes(x = ses, y = math)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE)
# ?coeftest

# singularity
# Singularity occurs when an element of your variance-covariance matrix 
# is estimated as essentially zero as a result of extreme multicollinearity 
# or because the parameter is actually essentially zero.

# example from Chapter 6, predicting math achievement from SES with a random slope:
# the correlation between our random effects is -1.00, a sign of perfect multicollinearity
ses_l1_random <- lmer(math ~ 1 + ses + (1 + ses|schcode), data = data, REML = TRUE)

Matrix::bdiag(VarCorr(ses_l1_random))

summary(ses_l1_random)

confint(ses_l1_random, oldNames = FALSE)

# removing that problematic covariance
ses_l1_random_cov0 <- lmer(math ~ 1 + ses + (1|schcode) + (0 + ses|schcode), data = data, REML = TRUE)
summary(ses_l1_random_cov0)


# Deviance Testing for Model Comparison
# we can still use REML estimator that more accurately estimates random effects
ses_l1 <- lmer(math ~ 1 + ses + (1|schcode), data = data, REML = TRUE)
ses_l1_random_cov0 <- lmer(math ~ 1 + ses + (1|schcode) + (0 + ses|schcode), data = data, REML = TRUE)


# deviance test to compare model fit
# Specifying refit = FALSE stops the function from refitting the models with FIML
anova(ses_l1, ses_l1_random_cov0, refit = FALSE)

















