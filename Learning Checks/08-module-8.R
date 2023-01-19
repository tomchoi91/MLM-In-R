# Load Data and Dependencies ----------------------------------------------

library(dplyr) # for data manipulation
library(magrittr) # for assignment pipe %<>%
library(lme4) # for multilevel models
library(lmerTest) # for p-values

data <- read.csv('./heck2011.csv', fileEncoding = "UTF-8-BOM")

# Options for Centering in MLMs -------------------------------------------
# This estimate cannot tell us about the effect within a school, 
# or the contextual effect, it is an uninterpretable blend of both. 
# We will walk through how to tease these different effects out 
# using centering.

model <- lmer(math ~ 1 + ses + (1|schcode), data = data, REML = TRUE)
summary(model)

# Centering Within Cluster (CWC) ------------------------------------------

data %<>% # this symbol is an assignment operator and pipe, equivalent to data <- data %>% 
  group_by(schcode) %>% 
  mutate(ses_mean = mean(ses))

data %<>%
  mutate(ses_cwc = ses - ses_mean)

data %>% 
  group_by(schcode) %>% 
  summarize(
    mean(ses_cwc)
  )

model_cwc <- lmer(math ~ 1 + ses_cwc + (1|schcode), data = data, REML = TRUE)
summary(model_cwc)

model_cwc_l2 <- lmer(math ~ 1 + ses_cwc + ses_mean + (1|schcode), data = data, REML = TRUE)
summary(model_cwc_l2)

# Centering Grand Mean (CGM) ----------------------------------------------
# CGM is appropriate when one is primarily interested 
# in a Level 2 predictor (cl) and wants to control for Level 1 covariates
# (do not include individual scores for variable of interest!)

data %<>%
  ungroup() %>% # remove the grouping by school that we added in the CWC section
  mutate(ses_grand_mean = mean(ses))

data %<>%
  mutate(ses_cgm = ses - ses_grand_mean)

data %>% 
  summarize(
    mean(ses_cgm)
  )

cgm_model <- lmer(math ~ 1 + ses_cgm + ses_mean + (1|schcode), data = data, REML = TRUE)
summary(cgm_model)

