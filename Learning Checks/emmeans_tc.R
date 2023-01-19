library(emmeans)
library(lme4)
library(magrittr)
if (!require(pander)){
  install.packages('pander')
  library(pander)
  }
# ?emmeans::contrast()

# Contrasts and followup tests using lmer
m <- lmer(Reaction~factor(Days)+(1|Subject), data=lme4::sleepstudy)
anova(m)
m.emm <- emmeans(m, "Days")
m.emm
m.emm.df <-
  m.emm %>%
  broom::tidy(conf.int = TRUE)
m.emm.df

# ?broom::tidy

m.emm.df %>%
  ggplot(aes(Days, estimate, ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() +
  ylab("RT")

confint(m)

# results not shown to save space
contrast(m.emm, 'tukey') %>%
  broom::tidy() %>%
  head(6)

# results not shown to save space
contrast(m.emm, 'trt.vs.ctrl', ref='0') %>%
  broom::tidy() %>%
  head()  %>%
  pander()

# results not shown to save space
contrast(m.emm, 'poly') %>%
  broom::tidy() %>%
  head(3) %>%
  pander(caption="The first three polynomial contrasts. 
         Note you'd have to have quite a fancy theory 
         to warrant looking at any of the higher level polynomial terms.")
