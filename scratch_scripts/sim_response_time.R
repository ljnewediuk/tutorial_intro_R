
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
# Simulate group-level data to explore random effects

set.seed(40)

# design
n_animals <- 20
n_obs <- 15

animal <- factor(rep(1:n_animals, each = n_obs))

# predictor: temperature (°C)
temp <- rnorm(n_animals * n_obs, mean = 20, sd = 5)

# random effects
# intercept variation (baseline neural speed)
b0 <- rnorm(n_animals, 0, 5)

# slope variation (temperature sensitivity)
b1 <- rnorm(n_animals, -2.5, 12)

# fixed effects
beta0 <- 100   # baseline response time (ms)
beta1 <- -1.3 # faster responses at higher temp

# generate response
response_time <- beta0 + 
  beta1 * temp + 
  b0[animal] + 
  b1[animal] * temp + 
  rnorm(n_animals * n_obs, 0, 30)

# Scale the response
rescale_variable <- function(x, r_min, r_max) {
  x_min <- min(x, na.rm = TRUE) # Calculate original min, ignoring NAs
  x_max <- max(x, na.rm = TRUE) # Calculate original max, ignoring NAs
  
  # Apply the scaling formula
  x_scaled <- r_min + (x - x_min) * (r_max - r_min) / (x_max - x_min)
  
  return(x_scaled)
}

# Rescale 
response_time_fgd_ms <- rescale_variable(x = response_time, r_min = 12, r_max = 28)

# Generate the data
# animal = individual animal ID
# temp = ambient temperature in degrees C
# response_time = response time delay before force generation delay in milliseconds
dat <- data.frame(animal = paste0("CX", animal), 
                  temp = round(temp, 1), 
                  response_time = round(response_time_fgd_ms, 2))

# Plot
ggplot(dat, aes(x = temp, y = response_time, colour = animal)) +
  geom_point() + 
  geom_smooth(method = 'lm')

# Try some models
lm_s <- lm(response_time ~ temp, data = dat)
i_mm <- lmer(response_time ~ temp + (1 | animal), data = dat)
is_mm <- lmer(response_time ~ temp + (temp | animal), data = dat)

summary(lm_s)
summary(i_mm)
summary(is_mm)

# Save data
write.csv(dat, "datasets/response_times.csv", row.names = F)


# Tasks: 
# 
# 1 - Get them to plot the indivdual effects in ggplot (they have to figure out
#     how to colour by individual)
#     
# 2 - Fit simple lm
# 
# 3 - Fit mixed models (random intercept/slopes)
#     Show fixed effects and variance of random intercepts/slopes across models
#     Extract the random intercepts/slopes and visualize them relative to the main effect
#     Look at correlation slope/intercept in intercept/slope model
# 
# 4 - Calculate marginal/conditional r2