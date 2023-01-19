library(tidyverse)
if (!require(GGally)){
  install.packages("GGally")
  library(GGally)
}
if (!require(simr)){
  install.packages("simr")
  library(simr)
}
library(lme4)

# create outcomes (y) with no relationship to x, 
# and no clustering by g at this stage (i.e. y, x and g are uncorrelated).

set.seed(1234)
simulated.df <- tibble(
  x = rep(0:1, 50),
  g = rep(1:10, 10),
  y = rnorm(100)
)

GGally::ggpairs(simulated.df)

# We can verify x is unrelated to outcome with lmerTest::anova
library(lmerTest)
model.of.interest <- lmer(y ~ x + (1|g), data=simulated.df)
# boundary (singular) fit: see ?isSingular
anova(model.of.interest)

# First, let’s change the ‘true’ effect of x to be 0.2:
fixef(model.of.interest)[2] <- .2

# alternative 1
model.of.interest@beta[2]<-.2

# alternative 2
y <- c(0.135833,0.200000)
names(y) <- c("(Intercept)", "x")
model2 <- simr::`fixef<-`(model.of.interest, y)

# str(lme4::fixef(model.of.interest)[2])
# lme4::fixef(model.of.interest)[2] <- .2
# simr::`fixef<-`(model.of.interest, lme4::fixef(model.of.interest))
# colnames(getME(model.of.interest, "X"))


powerSim(model.of.interest, test=lme4::fixef(model.of.interest)[2])
?powerSim

