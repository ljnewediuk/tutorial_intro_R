
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
# Simulate group-level data to explore random effects

set.seed(123)

# design
n_animals <- 25
n_obs <- 20

animal <- factor(rep(1:n_animals, each = n_obs))

# animal-level "preferred" temperature (drives confounding)
temp_mean_animal <- rnorm(n_animals, mean = 20, sd = 4)

# generate temp with within-animal variation
temp <- rep(temp_mean_animal, each = n_obs) + 
  rnorm(n_animals * n_obs, 0, 2)

# add condition variable
condition <- rnorm(n_animals * n_obs, mean = 0, sd = 1)

# random intercepts (baseline performance)
# IMPORTANT: correlate intercept with mean temp (confounding!)
b0 <- 0.8 * temp_mean_animal + rnorm(n_animals, 0, 2)

# random slopes (individual sensitivity)
b1 <- rnorm(n_animals, mean = -1.5, sd = 0.5)

# generate response
response_time <- 60 +
  b0[animal] +                  # intercept differences
  b1[animal] * temp +          # slope differences
  rnorm(n_animals * n_obs, 0, 2)


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

# Add poisson response data (number of trials required to complete a maze;
# decreases with temperature and condition)
eta <- 1.8 +
  b0[animal] +
  (-0.5 + b1[animal]) * temp +
  (-0.15) * condition

# convert to probability
lambda <- exp(eta + 10)

# generate outcome
trials_raw <- rpois(n_animals * n_obs, lambda)

# constrain to no more than 25 trials and greater than zero
trials <- pmin(trials_raw, 25) + 1

# Generate the data
# animal = individual animal ID
# temp = ambient temperature in degrees C
# response_time = response time delay before force generation delay in milliseconds
dat <- data.frame(animal = paste0("CX", animal), 
                  temp = round(temp, 1), 
                  response_time = round(response_time_fgd_ms, 2),
                  trials)

# Randomly remove some data
rmv <- sample(1:500, 147)
dat <- dat[- rmv,]

# Plot
ggplot(dat, aes(x = temp, y = response_time)) +
  geom_point(aes(colour = animal)) + 
  geom_smooth(method = 'lm', aes(colour = animal)) +
  geom_smooth(method = 'lm', colour = 'black')

# Try some models
lm_s <- lm(response_time ~ temp, data = dat)
i_mm <- lmer(response_time ~ temp + (1 | animal), data = dat)
is_mm <- lmer(response_time ~ temp + (1 + temp | animal), data = dat)

summary(lm_s)
summary(i_mm)
summary(is_mm)

AIC(lm_s)
AIC(i_mm)
AIC(is_mm)

# Save data
write.csv(dat, "datasets/response_times.csv", row.names = F)

# Get marginal effects, conditional effects, and residuals
mm_effects <- dat %>% mutate(fit.m = predict(is_mm, re.form = NA), 
                       fit.c = predict(is_mm, re.form = NULL),
                       resid = resid(is_mm))

# Fit from the simple lm (linear regression fit to all data + linear regression
# fit to each group unit)
ggplot(dat, aes(x = temp, y = response_time)) +
  geom_smooth(aes(colour = animal), se = F, method = 'lm', linewidth = 0.5) + 
  geom_smooth(colour = 'black', se = F, method = 'lm', linewidth = 0.5)

# Marginal fit, conditional fits, and conditional residuals
ggplot(mm_effects, aes(x = temp, y = fit.m + resid)) +
  geom_line(aes(y = fit.c, colour = animal)) + 
  geom_line(aes(y = fit.m), colour = 'black')

# Tasks: 
# 
# 1 - Get them to plot the indivdual effects in ggplot (they have to figure out
#     how to colour by individual). Then plot the predicted relationship using 
#     geom_smooth (classic model with only fixed effects). They will compare this
#     to the marginal effects they predict and plot from their mixed-effects model.
#     
#     * The conditional effects show the group-level effects of a predictor 
#       (what would I expect y to be, given x, within group z?)
#     * The marginal effect is the average effect of the predictor across all
#       groups, averaging the group-specific differences (what would I expect
#       y to be, given x, for the average group?)
#     
# 2 - Fit simple lm
# 
#     Compare to mixed model
#     Also fit a linear model with the group as a fixed effect and look at how 
#     many degrees of freedom are eaten up
#     use df.residual() to show how random effects share information across groups,
#     and thus do not need to use a large number of effect sizes, reducing degrees
#     of freedom. In the random effects model, we only need one degree of freedom
#     for the pooled variance of the random effect (more data available, so we
#     have more power to actually detect an effect)

# 3 - Fit mixed models (random intercept/slopes)

#     Review model summaries:
#     
#     Random effects:
#     * Intercept variance = how much groups differ in their intercepts
#     * Slope variance = how much groups differ in the effect of x on y
#     Ultimately, we want to know how much variance their is between groups in
#     terms of their basic differences in y (intercepts) and relationships
#     (slopes)
#
#     Residual variance:
#     This is the remaining variance in the model output that is unexplained by
#     either the fixed or random effects. 
#     * The smaller the number, the better the model. (quiz them on whether the 
#       intercept or intercept + slope model is better based on residual 
#       variance) This is basically the deviation of each individual observation 
#       from the group's predicted slope.
#     * If the residual variance is large relative to the random effect variance,
#       it suggests most of the variation is within groups rather than between
#       groups (relatively large random effects variance = differences between
#       groups). This only really works for lmer.
#     * Quiz them on whether most of the variation is within or between groups
#       in terms of slopes and intercepts

# 4 - Visualize the random effects

#     Extract the random intercepts/slopes and visualize them to show that they 
#     are relative to the main effect
#     Look at correlation slope/intercept in intercept/slope model (quiz them
#     on whether the two are correlated)

# 
# 5 - Calculate marginal/conditional r2

#     Marginal and conditional effects:
#     * Marginal effects = variance explained by the fixed effects only
#     * Conditional effects = variance explained by the fixed and random
#       effects
#     * Difference = importance of groups
#     * Quiz them on this concept for Poisson glmers