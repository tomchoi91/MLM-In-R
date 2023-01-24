library(lme4)
library(magrittr)
library(dplyr)

# aa <- lme4::sleepstudy
# str(aa)
random.intercepts.model <- lmer(Reaction ~ Days + (1|Subject),  data=lme4::sleepstudy)
summary(random.intercepts.model)
VarCorr(random.intercepts.model)

# Groups   Name        Std.Dev.
# Subject  (Intercept) 37.124  
# Residual             30.991  

m2 <- lmer(Reaction~Days+(1|Subject)+(0+Days|Subject),lme4::sleepstudy,REML=FALSE)
m1 <- update(m2,.~Days+(1|Subject))
m0 <- lm(Reaction~Days,lme4::sleepstudy)
anova(m2,m1,m0) ## two sequential tests

# m0a <- lmer(Reaction~Days+(1|Subject),lme4::sleepstudy)
# summary(m0a)
# m0b <- lmer(Reaction~1+(1|Subject),lme4::sleepstudy)
# summary(m0b)
# anova(m0a,m0b, refit=FALSE)
# anova(m0a,m0b, refit=TRUE)


VarCorr(random.intercepts.model) %>%
  as_tibble() %>%
  mutate(icc=vcov/sum(vcov))

# ?VarCorr
