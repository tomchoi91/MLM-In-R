library(lme4)
library(dplyr)

set.seed(789)
m <- 600  # 600 individual samples
J <- 20  # 20 groups

confint_factor <- 1.96

beta <- c(30, 2.5, 100)  # arbitrary fixed coefficients
X <- cbind(1,
           rnorm(m, 50, 25),
           rnorm(m, 100, 15))  # two observable predictor variables + intercept
Z <- matrix(rep(1, m))  # column of ones for random intercept
groupsizes <- rep(m/J, J)  # groups evenly sized
groups <- rep(LETTERS[1:J], groupsizes)

fixed_part <- X %*% beta

mean_delta <- 0
sd_delta <- 5
mean_epsilon <- 0
sd_epsilon <- 5
n_runs <- 1000
models <- as.list(rep(NA, n_runs))
deltas <- as.list(rep(NA, n_runs))
fixefs <- as.list(rep(NA, n_runs))
ranefs <- as.list(rep(NA, n_runs))
idx<-1

# for (idx in 1:n_runs) {
  delta <- rnorm(J, mean = mean_delta, sd = sd_delta)
  delta_long <- matrix(rep(delta, groupsizes))
  
  epsilon <- rnorm(m, mean = mean_epsilon, sd = sd_epsilon)
  Y <- fixed_part + Z * delta_long + epsilon
  # Z2 <- Z*delta_long
  model <- lmer(Y ~ X - 1 + (Z - 1 | groups))
  models[[idx]] <- model
  deltas[[idx]] <- delta
  ### saving ranefs:
  ranefs[[idx]] <-
    as.data.frame(ranef(model)) %>%
    mutate(true_value = delta,
           lower = condval - confint_factor * condsd,
           upper = condval + confint_factor * condsd) %>%
    mutate(true_val_in_confint = true_value >= lower & true_value <= upper)
  ### saving fixefs:
  my_coefs <- summary(model)$coefficients
  fixefs[[idx]] <-
    summary(model)$coefficients %>%
    as_tibble() %>%
    mutate(true_value = beta,
           lower = Estimate - confint_factor * `Std. Error`,
           upper = Estimate + confint_factor * `Std. Error`) %>%
    mutate(true_val_in_confint = true_value >= lower & true_value <= upper)
# }

deltas_in_range <-
  ranefs %>%
  sapply(FUN = function(x) x[['true_val_in_confint']]) %>%
  rowSums()

betas_in_range <-
  fixefs %>%
  lapply(as.data.frame) %>%
  sapply(FUN = function(x) x$true_val_in_confint) %>%
  rowSums()

print(deltas_in_range)
## [1] 772 781 793 785 788 779 805 791 800 766 803 780 781 775 791 788 788 788 778 784
print(betas_in_range)  
## [1] 954 944 948