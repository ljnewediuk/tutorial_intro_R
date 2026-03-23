
# 1- Simulate multicollinearity ====

# Students model the effect of total winter snowfall and average spring 
# temperature on crop yield (wheat, in bushels/acre) in 60 farms from 2021-2025.

# Snowfall and temperature both significantly increase yield. However, years
# with little snowfall tend to have higher average temperature in the spring.

# If they include both, there is multicollinearity and neither snowfall nor
# temperature has an effect.

set.seed(1)
n <- 60   # Number of farms

# Generate variables
snow_cm <- rnorm(n, 50, 10)
temp_C <- 50 - snow_cm + rnorm(n, 0, 0.5)   # strongly correlated
yield_ba <- 3*snow_cm + 3*-temp_C + rnorm(n,0,10)

# Data frame
dat_multi <- data.frame(
  snow_cm,
  temp_C,
  yield_ba
)

# Exclude negative yields
dat_multi <- dat_multi[dat_multi$yield > 0 ,]

# Round variables
dat_multi_r <- as.data.frame(apply(dat_multi, 2, function(x) round(x, 1)))

cor(dat_multi_r)

# Correct model (total effect of oxygen)
m1 <- lm(yield_ba ~ snow_cm, data = dat_multi_r)
summary(m1)

# Correct model (total effect of algae)
m2 <- lm(yield_ba ~ temp_C, data = dat_multi_r)
summary(m2)

# Biased model
m3 <- lm(yield_ba ~ snow_cm + temp_C, data = dat_multi_r)
summary(m3)

# Save the data
write.csv(dat_multi_r, 'crop_yields.csv', row.names = F)

# 2 - Simulate post-treatment bias ====

# Students now model the effect of fertilizer on plant size and fruit number in
# tomato plants. 

# Fertilizer increases plant size and fruit number, and bigger plants produce 
# more fruit. However, if they include both fertilizer and plant size (control
# for plant size), fertilizer will appear to have no effect on fruit number.

set.seed(2)
n <- 50

# Generate variables
fertilizer_mg <- rnorm(n, 50, 10)
plant_height_cm <- .25*fertilizer_mg + rnorm(n,0,1)
# Generate linear model
loglambda <- 0.02*fertilizer_mg + 0.005*plant_height_cm
lambda <- exp(loglambda)
# Generate poisson variable
fruit_count <- rpois(n, lambda = lambda)

# Data frame
dat_post <- data.frame(
  fertilizer_mg,
  plant_height_cm,
  fruit_count
)

# Round first two variables
dat_post_r <- as.data.frame(apply(dat_post[,1:2], 2, function(x) round(x, 1)))
dat_post_r$fruit_count <- dat_post$fruit_count

# Correct model (total effect of fertilizer)
m1 <- glm(fruit_count ~ fertilizer_mg,
          data = dat_post_r,
          family = poisson)

summary(m1)

# Biased model (treatment has no effect)
m2 <- glm(fruit_count ~ fertilizer_mg + plant_height_cm,
          data = dat_post_r,
          family = poisson)

summary(m2)

# Save the data
write.csv(dat_post_r, 'tomatoes.csv', row.names = F)

# 3 - Simulate collider bias ====

# Students want to know the relationship between wine price and quality (are 
# higher-priced bottles better quality?)

# Bottle price affects critical acclaim score. Quality also affects critical
# acclaim score. The students condition on acclaim score to test the association
# between price and quality, which induces an association, even though their is
# none.

# Students measure toxin production and want to know what affects it.
# But they control for growth rate. Growth rate is affected by both temperature 
# and nutrients → collider.

# DAG
# price → acclaim ← quality

set.seed(3)
n <- 80

# Generate variables
quality <- rnorm(n,8,1)
price <- rnorm(n,70,15)
acclaim <- -75 + 8*quality + 0.8*price + rnorm(n,0,3)

# Data frame with rounding
dat_coll <- data.frame(
  quality_score = round(quality, 1),
  price_CAD = round(price, 2),
  acclaim = round(acclaim, 0)
)

# Save the data
write.csv(dat_coll, 'wine_quality.csv', row.names = F)

# Correct model (no association)
m1 <- lm(quality_score ~ price_CAD, data = dat_coll)

summary(m1)

# Correct model (acclaim increases with quality)
m2 <- lm(acclaim ~ quality_score, data = dat_coll)

summary(m2)

# Biased model (conditioned on the collider)
m3 <- lm(quality_score ~ price_CAD + acclaim, data = dat_coll)

summary(m3)
