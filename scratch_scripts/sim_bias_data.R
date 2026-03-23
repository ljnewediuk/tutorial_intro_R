
# 1- Simulate multicollinearity ====

# Students model number of dead fish/microbial growth across lakes.
# They think oxygen matters, but oxygen is strongly correlated with algae biomass.
# Both affect microbial growth, which affects dead fish.

# They include both → multicollinearity (and oxygen has no apparent effect.

set.seed(1)
n <- 60   # lakes

# Generate variables
oxygen <- rnorm(n, 8, 1)
algae <- oxygen + rnorm(n, 0, 0.3)   # strongly correlated
microbe <- 2*oxygen + 2*algae + rnorm(n,0,2)
dead_fish <- 5 + 3*microbe + rnorm(n,0,5)

# Data frame
dat_multi <- data.frame(
  oxygen,
  algae,
  microbe,
  dead_fish
)

cor(dat_multi)

# Correct model (total effect of oxygen)
m1 <- lm(microbe ~ oxygen, data = dat_multi)
summary(m1)

# Correct model (total effect of algae)
m2 <- lm(microbe ~ algae, data = dat_multi)
summary(m2)

# Biased model
m3 <- lm(microbe ~ oxygen + algae, data = dat_multi)
summary(m3)

# 2 - Simulate post-treatment bias ====

# They culture the microbe and test a chemical treatment.

# Process:
# Treatment → reduces growth
# Growth → increases toxin
# Toxin → kills fish cells

# We want:
#   Does treatment improve survival?
#   Students mistakenly control for toxin.

set.seed(2)
n <- 500

# Generate variables
treatment <- rbinom(n, 1, 0.5)
growth <- 2 - 1.2*treatment + rnorm(n,0,1)
toxin <- 1.5*growth + rnorm(n,0,1)
linpred <- 1.0*treatment - 1.2*toxin
prob_surv <- plogis(linpred)
survival <- rbinom(n, 1, prob_surv)

# Data frame
dat_post <- data.frame(
  treatment,
  growth,
  toxin,
  survival
)

# Correct model (total effect of treatment)
m1 <- glm(survival ~ treatment,
          data = dat_post,
          family = binomial)

summary(m1)

# Biased model (treatment has no effect)
m2 <- glm(survival ~ treatment + toxin,
          data = dat_post,
          family = binomial)

summary(m2)

# 3 - Simulate collider bias ====

# Different microbial cultures vary in toxicity

# Toxicity depends on:
  # Temperature
  # Nutrient concentration

# Students measure toxin production and want to know what affects it.
# But they control for growth rate. Growth rate is affected by both temperature 
# and nutrients → collider.

# DAG
# mutation → growth ← nutrients
# growth → toxin
# mutation → toxin

set.seed(3)
n <- 500

# Generate variables
temperature <- rnorm(n,0,1)
nutrient <- rnorm(n,0,1)
growth <- 1.2*temperature + 1.2*nutrient + rnorm(n,0,1)
toxin <- 1.5*temperature + 0.0*nutrient + 1.0*growth + rnorm(n,0,1)

dat_coll <- data.frame(
  temperature,
  nutrient,
  growth,
  toxin
)

# Correct model
m1 <- lm(toxin ~ temperature + nutrient, data = dat_coll)

summary(m1)

# Biased model (conditioned on the collider)
m2 <- lm(toxin ~ temperature + nutrient + growth, data = dat_coll)

summary(m2)




