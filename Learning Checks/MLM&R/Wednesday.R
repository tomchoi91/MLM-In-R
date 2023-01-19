setwd('C:/Users/wxu17/Desktop/MLM&R')

data2 <- read.csv('heck2011.csv')
summary(data)

install.packages('lme4')
install.packages('lmerTest')
install.packages('performance')
install.packages('ggplot2')
install.packages('emmeans')

library(lme4)
library(lmerTest)
library(performance)
library(ggplot2)
library(magrittr)
install.packages('emmeans')
library('emmeans')
install.packages('pbkrtest')
library('pbkrtest')
install.packages("pander")
library('pander')

#looking at the effect of Days of sleep deprivation on reaction times:
m <- lmer(Reaction~factor(Days)+(1|Subject), data=lme4::sleepstudy)
anova(m)
#We can see a significant effect of Days in the Anova table, and want to compute followup tests.
#To first estimate cell means and create an emmeans object
m.emm <- emmeans(m, "Days")
?emmeans
m.emm
#It might be nice to extract these estimates and plot them:
m.emm.df <-
  m.emm %>%
  broom::tidy(conf.int=TRUE)

m.emm.df %>%
  ggplot(aes(Days, estimate, ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() +
  ylab("RT")
#---------------------------------------------------------------------------
#7.2.6 Deviance Testing for Model Comparison
##Here, we’re comparing models with the same fixed effects but different random effects so we can still use REML estimator that more accurately estimates random effects. 

### models
ses_l1 <- lmer(math ~ 1 + ses + (1|schcode), data = data, REML = TRUE)
ses_l1_random_cov0 <- lmer(math ~ 1 + ses + (1|schcode) + (0 + ses|schcode), data = data, REML = TRUE)

### deviance test to compare model fit
####Specifying refit = FALSE stops the function from refitting the models with FIML. If we were comparing models with different fixed effects, we would use FIML to estimate our models.
anova(ses_l1, ses_l1_random_cov0, refit = FALSE)

#-----------------------------------------------------------------------------
#-----Chapter 8 Centering Options and Interpretations
library(dplyr) # for data manipulation
library(magrittr) # for assignment pipe %<>%
library(lme4) # for multilevel models
library(lmerTest) # for p-values

data <- read.csv('heck2011.csv')

##Let’s return to our example of SES predicting math achievement to understand if there is a contextual effect of students’ achievement from being in higher or lower on average SES schools. So far, we’ve been using the following model:

model <- lmer(math ~ 1 + ses + (1|schcode), data = data, REML = TRUE)
summary(model)

#If we want to get our within, between, and contextual effects, we have two options (Enders and Tofighi, 2007):

Centering within cluster (CWC)
Centering around grand mean (CGM).

##Centering within cluster (CWC)
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
##we estimate a model with just ses_cwc
model_cwc <- lmer(math ~ 1 + ses_cwc + (1|schcode), data = data, REML = TRUE)
summary(model_cwc)
##This model is incomplete. If we want our between effect (i.e., how the school averages differ from each other), we can add the aggregate back in at level 2, which is the value we calculated for each school’s mean, ses_mean:
model_cwc_l2 <- lmer(math ~ 1 + ses_cwc + ses_mean + (1|schcode), data = data, REML = TRUE)
summary(model_cwc_l2)
##with two hypothetical students with the same level of SES, the one in the school with higher average SES has 2.70-unit higher math achievement. This represents the contextual effect of a school’s SES on math achievement.

##Centering around grand mean (CGM)
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

#############-------------Practice-------------------#######################
#Problem 1
install.packages('pacman')
library('pacman')

data <- read.csv('AbOpinion.csv')
data %<>%
  mutate(female = Sex)

##conditional model
m1 <- lmer(Support ~ 1 + factor(female) + Ages + factor(Religion) + (1|Districts), data = data, REML = TRUE)
summary(m1)
anova(m1)

#To first estimate cell means and create an emmeans object
m.emm <- emmeans(m1, "female")
m.emm
#It might be nice to extract these estimates and plot them:
m.emm.df <-
  m.emm %>%
  broom::tidy(conf.int=TRUE)

m.emm.df %>%
  ggplot(aes(female, estimate, ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() +
  ylab("Support")

#To first estimate cell means and create an emmeans object
m.emm <- emmeans(m1, "Religion")
m.emm
#It might be nice to extract these estimates and plot them:
m.emm.df <-
  m.emm %>%
  broom::tidy(conf.int=TRUE)

m.emm.df %>%
  ggplot(aes(Religion, estimate, ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() +
  ylab("Support")

#to see the contrast
aa <- contrast(m.emm, 'tukey') %>%
  broom::tidy() %>%
  head(6)


bb <- contrast(m.emm, 'trt.vs.ctrl') %>%
  broom::tidy() 
  head %>%
  pander

cc <-contrast(m.emm, 'poly') %>%
  broom::tidy() %>%
  head(3) %>%
  pander(caption="The first three polynomial contrasts. Note you'd have to have quite a fancy theory to warrant looking at any of the higher level polynomial terms.")

#Problem 2
m2 <- lmer(Support ~ 1 + factor(female) + Ages + factor(SocClass) + factor(Religion) + (1|Districts), data = data, REML = TRUE)
summary(m2)

##deviance test
anova(m1, m2, refit = T)
##What is the number of parameters in each model? 
8, 10
##What is the difference in degrees of freedom?
2
##Which model had lower deviance values (AIC, BIC, and deviance)? 
m1
##Did the model *significantly* improve when respondent's social class was included as a predictor?
no


#problem3

##Centering within cluster (CWC)
data %<>% # this symbol is an assignment operator and pipe, equivalent to data <- data %>% 
  group_by(Districts) %>% 
  mutate(age_mean = mean(Ages))
data %<>%
  mutate(age_cwc = Ages - age_mean)
data %>% 
  group_by(Districts) %>% 
  summarize(
    mean(age_cwc)
  )

##Centering around grand mean (CGM)
data %<>%
  ungroup() %>% 
  mutate(age_grandmean = mean(Ages))
data %<>%
  mutate(age_cgm = Ages - age_grandmean)
data %>% 
  summarize(
    mean(age_cgm)
  )


m3 <- lmer(Support ~ 1 + factor(female) + age_cwc + age_mean + factor(Religion) + (1|Districts), data = data, REML = TRUE)
summary(m3)


##A one-unit increase in age *within* a district is associated with what amount of change in support of women's reproductive rights? 
-6.218e-03
##A one-unit increase in the *district's average age* is associated with what amount of change in support of women's reproductive rights? 
2.879e-02


m4 <- lmer(Support ~ 1 + factor(female) + age_cgm + age_mean + factor(Religion) + (1|Districts), data = data, REML = TRUE)
summary(m4)

##A one-unit increase in age *within* a district relative to the grand mean is associated with what amount of change in support of women's reproductive rights? 
-6.218e-03
##For two people who are of the same age, what is the amount of change in support of women's reproductive rights for the person who lives in a district of higher average age? That is, for every 1 unit increase in CGM age, what is the associated change in their support of reproductive rights?
3.501e-02

#problem 4
p_load("GGally", "simr", "dplyr", "lme4", "lmerTest", install = T)
set.seed(1111)
sim_data <- data.frame(
  x = rep(0:1, 10), # Binary predictor/explanatory variable; either 0 or 1
  g = rep(1:10, 20), # 20 clusters; between 1 and 10
  y = rnorm(100) # 100 values of outcome variable from normal distribution
)
ggpairs(sim_data)

##Create a multilevel model using `lmer()` where y is the outcome, x is the predictor, and g is the grouping variable. Also include a random intercept. 
m5 <- lmer(y ~ x + 1 + (1|g), data = sim_data, REML = T)
summary(m5)
##Use `powerSim` to compute the observed power of a fixed effect of 0.35 by using the following code: 
fixef(m5)['x'] <- .35
powerSim(m5, nsim = 100, progress = F)
##Increase the fixed effect to be 0.70 and calculate the observed power.
fixef(m5)['x'] <- .7
powerSim(m5, nsim = 100, progress = F)

##How does increasing the fixed effect impact observed power?

##Increase the variance of clustering parameter, *g*, to 0.4 and re-calculate the observed power. This creates an ICC = 0.29, where $\frac{\tau^2_0}{\tau^2_0+\sigma^2_0} = \frac{0.4}{0.4 + 1} = 0.2857$. Continue using a fixed effect of 0.70.


VarCorr(model)['g'] <- .4
powerSim(model, nsim = 100, progress = F)


##How does increasing the variability between clusters impact observed power?







