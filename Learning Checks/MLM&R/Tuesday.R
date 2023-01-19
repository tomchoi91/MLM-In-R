library('lme4')
library('lmerTest')
library('performance')
library('dplyr')
library('ggplot2')
install.packages('readxl')
library('readxl')

setwd('/Users/carol/desktop/MLM&R')

data <- read.csv('heck2011.csv')
summary(data)

data_sub <- data %>%
  filter(schcode <= 10)

data_sub %>%
  ggplot(mapping = aes (x = ses, y = math)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE)

#coloring the points by school(schocode)
data_sub %>%
  ggplot(mapping = aes (x = ses, y = math, colour = factor(schcode))) +
  geom_point() +
  geom_smooth(mapping = aes(group = schcode), method = "lm", se = FALSE, fullrange = TRUE) +
  labs(color = "schcode")

# the null model
null_model <- lmer(math ~ 1+(1|schcode), data=data)
summary(null_model)
##count the parameters
logLik(null_model)
### so we conclude that The fixed effect for the intercept is 57.67, 
###representing the average math achievement score across all schools. 
###The variance of schools around the intercept is 10.64, and of students around their school’s mean is 66.55.


#calculate ICC
performance::icc(null_model)
##the adjusted ICC accounts only for the random effect variances, while the conditional ICC accounts for the fixed effect variances, too.

#Another way to understand the variance in our data is by calculating a 95% plausible values range for a given effect. For example, the intercept: given the fixed effect for the intercept ( 
#γ
#00
#) and the variance of residuals around that fixed effect ( 
#  τ
#  2
#  0
#), we can describe how much the schools vary in mean math achievement by calculating a 95% plausible values range.

tau0 <- VarCorr(null_model)$schcode[1]
lower_bound <- null_model@beta - 1.96*sqrt(tau0)
upper_bound <- null_model@beta + 1.96*sqrt(tau0)

lower_bound
upper_bound

#MLM with level-1 predictor
null_model <- lmer(math ~ 1 + (1|schcode), data = data)
summary(null_model)
##add a fixed effect for ses as a predictor to our model
ses_l1 <- lmer(math ~ ses + (1|schcode), data = data, REML = TRUE)
summary(ses_l1)

null <- sigma(null_model)^2
l1 <- sigma(ses_l1)^2

(null - l1) / null
###So we reduced about 5.6% of level-1 variance by adding ses as a level-1 predictor. 

performance::icc(ses_l1)
###After accounting for the effect of socioeconomic status, 4.6% of the variance in math achievement is accounted for by school membership.


#Compare Regular and Multilevel Regression
##The regular regression
model <- lm(math ~ ses, data = data)
summary(model)
##The cluster-robust standard error regression
model_crse <- lmtest::coeftest(model, vcov = sandwich::vcovCL, cluster = ~ schcode)
model_crse
##These two models had the same coefficients, with different significance values.
##our MLM model:
summary(ses_l1)


#MLM with Level-2 Predictor
#Let’s consider the role of school type in our model by adding a fixed effect for public as a predictor of our intercept.
ses_l1_public_l2 <- lmer(math ~ 1 + ses + public + (1|schcode), data = data, REML = TRUE)
summary(ses_l1_public_l2)
##From our random effects, the variance term describing how schools vary around the intercept (at mean SES at private schools) is 3.48, and the variance term describing how students vary around their school means is 62.81.

##calculate variance reduced at level 1 and level 2 by adding school type as a predictor.
### level-1 variance reduced
sigma2_null <- sigma(null_model)^2
sigma2_public <- sigma(ses_l1_public_l2)^2
(sigma2_null - sigma2_public) / sigma2_null
### level-2 variance reduced
tau2_null <- VarCorr(null_model)$schcode[1]
tau2_public <- VarCorr(ses_l1_public_l2)$schcode[1]
(tau2_null - tau2_public) / tau2_null
##We reduced around 5.6% of variance in math achievement at level-1 and 67.2% of variance at level-2 by adding public as a level-2 predictor. It makes sense that the variance at level-2 was reduced by so much more, because we added a level-2 predictor that varies at level-2.

#-------------practice problems-----------------#
setwd('/Users/carol/desktop/MLM&R/Tuesday')

data_new <- read_xlsx('NE_EdSpending_Jan28th_22.xlsx', na='NA')
summary(data_new)

knitr::opts_chunk$set(echo = TRUE)

##Problem 1
data_new1 <- data_new |>
  group_by(district_ID) |>
  filter(n() > 1) 

#library('tidyr')
data_new2 <- data_new1 |> drop_na(pp_total_raw_NE)

data_new2 %>% 
  ggplot(mapping = aes(x = enroll_raw_NE, y = pp_total_raw_NE, group = factor(district_ID))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size =.1, fullrange = FALSE)+
  labs(color = "district_ID")

##problem 2
null_model <- lmer(pp_total_raw_NE ~ 1+(1|district_ID), data=data_new)
summary(null_model)
##count the parameters
logLik(null_model)

performance::icc(null_model)

VarCorr(null_model)

tau0 <- VarCorr(null_model)$district_ID[1]
lower_bound <- null_model@beta - 1.96*sqrt(tau0)
upper_bound <- null_model@beta + 1.96*sqrt(tau0)

lower_bound
upper_bound

###How many parameters were estimated (i.e., how many random and fixed effects)? 
3
###What is the fixed effect of the intercept? 
18291.1
###What is the variance of districts around this intercept?
25346146
###What is the variance of schools around their district's mean?
17875427

##problem 3

###1. What is the variance attributed to district ($\tau^2_0$)? 
25346146
###2. What is variance attributed to schools/students ($\sigma^2_0$)?
17875427
###3. What is the total variance ($\tau^2_0 + \sigma^2_0 = total$)?
25346146 + 17875427
###4. What is the percent of total variance attributed to district membership?
25346146/(25346146 + 17875427) #= ICC
###5. What is the percent of total variance attributed to school membership?
17875427/(25346146 + 17875427) #=1-ICC
###6. Compute the intraclass correlation (ICC) by hand. In laymen terms, this would be the (*variance attributed to district*) / (*total variance*). Here is the equation to do so: 
performance::icc(null_model)


tau0 <- VarCorr(null_model)$district_ID[1]
tao0
lower_bound <- null_model@beta - 1.96*sqrt(tau0)
upper_bound <- null_model@beta + 1.96*sqrt(tau0)

lower_bound
upper_bound

##problem 4
p4 <- lmer(pp_total_raw_NE ~ 1 + level + (1|district_ID), data = data_new, REML = TRUE)
summary(p4)

performance::icc(p4)

### level-1 variance reduced
sigma2_null <- sigma(null_model)^2
sigma2_public <- sigma(p4)^2
(sigma2_null - sigma2_public) / sigma2_null #=0.1987676

performance::icc(p4)
p4
#Now that you have entered a fixed effect into the model, reference the conditional (adjusted) ICC. What percent of total per-pupil expenditures is accounted for by grade level?
# after accounting for the shcool level, 59.6% of total per-pupil expenditures is accounted for by grade level. 
# so still 59.6% variances in level 2

##problem extra
extra <- lmer(pp_total_raw_NE ~ 1 + enroll_raw_NE + level + (1|district_ID), data = data_new, REML = TRUE)
summary(extra)


##calculate variance reduced at level 1 and level 2 by adding school type as a predictor.
### level-1 variance reduced
sigma2_null <- sigma(null_model)^2
sigma2_public <- sigma(extra)^2
(sigma2_null - sigma2_public) / sigma2_null

#compared with the last model "p4"
# still reduced: 0.2154257 - 0.1987676 = 0.1987676

performance::icc(extra)











