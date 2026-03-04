
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
                 sd = 20)

# Sampling area (slightly variable)
area <- runif(n, 0.8, 1.2)

# True coefficients
beta0 <- -2.5
beta1 <- 0.4      # fertilization increases abundance
beta2 <- 0.009    # biomass effect

# Linear predictor (include area as offset)
log_mu <- beta0 + 
  beta1 * fertilized + 
  beta2 * biomass + 
  log(area)

mu <- exp(log_mu)

# Simulate counts
visits <- rpois(n, lambda = mu)

pollinator_data <- data.frame(
  visits,
  fertilized = factor(fertilized),
  biomass = round(biomass, 1),
  area = round(area, 1)
)

head(pollinator_data)

# Save the data
write.csv(pollinator_data, "datasets/pollen_data.csv", row.names = FALSE)

poiss_lm <- lm(visits ~ biomass, data = pollinator_data)
poiss_glm <- glm(visits ~ biomass, data = pollinator_data, family = "poisson")

summary(poiss_lm)
summary(poiss_glm)

# Plot model predictions

# New age data
biomass_new <- seq(min(pollinator_data$biomass), max(pollinator_data$biomass), length.out = 80)

# Predictions
preds_lm <- predict(object = poiss_lm, newdata = data.frame(biomass = biomass_new), se.fit = T, type = "response")
preds_glm <- predict(object = poiss_glm, newdata = data.frame(biomass = biomass_new), se.fit = T, type = "response")

# Plot glm
poiss_glm_preds <- tibble(biomass = biomass_new, visits = preds_glm$fit, visits.se = preds_glm$se.fit)

poiss_glm_preds %>%
  ggplot(aes(x = biomass, y = visits)) +
  geom_ribbon(aes(ymin = visits - visits.se, ymax = visits + visits.se), alpha = 0.5) +
  geom_line() +
  geom_point(data = pollinator_data, aes(x = biomass, y = visits))

# Plot lm
poiss_lm_preds <- tibble(biomass = biomass_new, visits = preds_lm$fit, visits.se = preds_lm$se.fit)

poiss_lm_preds %>%
  ggplot(aes(x = biomass, y = visits)) +
  geom_ribbon(aes(ymin = visits - visits.se, ymax = visits + visits.se), alpha = 0.5) +
  geom_line() +
  geom_point(data = pollinator_data, aes(x = biomass, y = visits))

# Model comparison
AIC(poiss_glm)
AIC(poiss_lm)

# Binomial dataset
# Embryo implantation success depending on maternal age/hormone treatment



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

ivf_data <- data.frame(
  pregnant,
  age = floor(age),
  dose = round(dose, 1),
  prev_failure = factor(prev_failure)
)

head(ivf_data)

# Save the data
write.csv(ivf_data, "datasets/ivf_data.csv", row.names = FALSE)

# Model
bin_glm <- glm(pregnant ~ age, data = ivf_data, family = "binomial")
bin_lm <- lm(pregnant ~ age, data = ivf_data)

# Plot model predictions

# New age data
ages_new <- seq(min(ivf_data$age), max(ivf_data$age), length.out = 100)

# Predictions
preds_lm <- predict(object = bin_lm, newdata = data.frame(age = ages_new), se.fit = T, type = "response")
preds_glm <- predict(object = bin_glm, newdata = data.frame(age = ages_new), se.fit = T, type = "response")

# Plot glm
bin_glm_preds <- tibble(age = ages_new, pregnant = preds_glm$fit, pregnant.se = preds_glm$se.fit)

bin_glm_preds %>%
  ggplot(aes(x = age, y = pregnant)) +
  geom_ribbon(aes(ymin = pregnant - pregnant.se, ymax = pregnant + pregnant.se), alpha = 0.5) +
  geom_line() +
  geom_point(data = ivf_data, aes(x = age, y = pregnant))

# Plot lm
bin_lm_preds <- tibble(age = ages_new, pregnant = preds_lm$fit, pregnant.se = preds_lm$se.fit)

bin_lm_preds %>%
  ggplot(aes(x = age, y = pregnant)) +
  geom_ribbon(aes(ymin = pregnant - pregnant.se, ymax = pregnant + pregnant.se), alpha = 0.5) +
  geom_line() +
  geom_point(data = ivf_data, aes(x = age, y = pregnant))

# Model comparison
AIC(bin_glm)
AIC(bin_lm)

