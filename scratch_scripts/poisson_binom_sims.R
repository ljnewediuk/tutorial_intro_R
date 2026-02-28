
library(tidyverse)

# Poisson data
# Pollinator visits

set.seed(123)

n <- 80

# Treatment
fertilized <- rbinom(n, 1, 0.5)

# Biomass (fertilized plots have higher biomass)
biomass <- rnorm(n, 
                 mean = 300 + 80 * fertilized, 
                 sd = 40)

# Sampling area (slightly variable)
area <- runif(n, 0.8, 1.2)

# True coefficients
beta0 <- -2.5
beta1 <- 0.4      # fertilization increases abundance
beta2 <- 0.005    # biomass effect

# Linear predictor (include area as offset)
log_mu <- beta0 + 
  beta1 * fertilized + 
  beta2 * biomass + 
  log(area)

mu <- exp(log_mu)

# Simulate counts
pollinator_visits <- rpois(n, lambda = mu)

poisson_data <- data.frame(
  pollinator_visits,
  fertilized = factor(fertilized),
  biomass,
  area
)

head(poisson_data)

lm_mod <- lm(pollinator_visits ~ biomass, data = poisson_data)


# Binomial dataset
# Embryo implantation success depending on maternal age/hormone treatment

set.seed(101)

n <- 150  # number of patients

# Predictors
age <- rnorm(n, mean = 34, sd = 4)
dose <- rnorm(n, mean = 225, sd = 35)
prev_failure <- rbinom(n, 1, 0.4)

# True coefficients (logit scale)
alpha0 <- 3.5
alpha1 <- -0.10      # age reduces success
alpha2 <- 0.004      # higher dose increases success
alpha3 <- -1.2       # previous failure reduces success

# Linear predictor
logit_p <- alpha0 +
  alpha1 * age +
  alpha2 * dose +
  alpha3 * prev_failure

# Convert to probability
p <- plogis(logit_p)

# Simulate pregnancy outcome (0/1)
pregnant <- rbinom(n, size = 1, prob = p)

ivf_data <- data.frame(
  pregnant,
  age,
  dose,
  prev_failure
)

head(ivf_data)

# Binomial versus gaussian result in different conclusions

set.seed(222)

n <- 200

# Predictors
age <- rnorm(n, mean = 35, sd = 5)
dose <- rnorm(n, mean = 220, sd = 30)
prev_failure <- rbinom(n, 1, 0.4)

# Create nonlinear threshold effect for dose
high_dose <- ifelse(dose > 230, 1, 0)

# True coefficients (logit scale)
alpha0 <- 8
alpha1 <- -0.20        # strong age decline
alpha2 <- 1.2          # high dose substantially increases odds
alpha3 <- -1.5         # prior failure strongly reduces odds

# Linear predictor
logit_p <- alpha0 +
  alpha1 * age +
  alpha2 * high_dose +
  alpha3 * prev_failure

# Convert to probability
p <- plogis(logit_p)

# Simulate pregnancy outcome
pregnant <- rbinom(n, size = 1, prob = p)

ivf_mismatch <- data.frame(
  pregnant,
  age,
  dose,
  prev_failure = factor(prev_failure)
)

head(ivf_mismatch)

# Model
bin_glm <- glm(pregnant ~ age, data = ivf_mismatch, family = "binomial")
bin_lm <- lm(pregnant ~ age, data = ivf_mismatch)

# Plot model predictions

# New age data
nd <- tibble(age = seq(min(ivf_mismatch$age), max(ivf_mismatch$age), length.out = 100))

# Predictions
preds_lm <- predict(object = bin_lm, newdata = nd, se.fit = T, type = "response")
preds_glm <- predict(object = bin_glm, newdata = nd, se.fit = T, type = "response")

# Plot glm
bin_glm_preds <- tibble(age = seq(min(ivf_mismatch$age), max(ivf_mismatch$age), length.out = 100), pregnant = preds_glm$fit, pregnant.se = preds_glm$se.fit)

bin_glm_preds %>%
  ggplot(aes(x = age, y = pregnant)) +
  geom_ribbon(aes(ymin = pregnant - pregnant.se, ymax = pregnant + pregnant.se), alpha = 0.5) +
  geom_line() +
  geom_point(data = ivf_mismatch, aes(x = age, y = pregnant))

# Plot lm
bin_lm_preds <- tibble(age = seq(min(ivf_mismatch$age), max(ivf_mismatch$age), length.out = 100), pregnant = preds_lm$fit, pregnant.se = preds_lm$se.fit)

bin_lm_preds %>%
  ggplot(aes(x = age, y = pregnant)) +
  geom_ribbon(aes(ymin = pregnant - pregnant.se, ymax = pregnant + pregnant.se), alpha = 0.5) +
  geom_line() +
  geom_point(data = ivf_mismatch, aes(x = age, y = pregnant))

