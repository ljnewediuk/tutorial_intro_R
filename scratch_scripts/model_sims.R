
# Simulated data: Lizard running speeds in different habitats. The students
# will try a series of linear models (tests of difference and regressions) that
# meet and violate the assumptions of OLS/ANOVA

library(tidyverse)

# Set random seed
set.seed(2213)

# Number of observations
n <- 210

# Start basic data frame
lizard_dat <- tibble(
  sex = sample(c("Female", "Male"), n, replace = TRUE),
  habitat = sample(c("Forest", "Open"), n, replace = TRUE),
  age_years = runif(n, 1, 6)
)

# ------------------------------------------------------------
# (1) Add body size: sex difference + age relationship (MEETS ASSUMPTIONS)
# ------------------------------------------------------------

lizard_dat <- lizard_dat %>%
  mutate(
    
    body_size_mm =
      ifelse(sex == "Female",
             
             80 + 3.8 * age_years +   # Females grow faster
               rnorm(n(), 0, 2.5),
             
             82 + 2.2 * age_years +   # Males grow slower
               rnorm(n(), 0, 3.5)
      )
  )

# ------------------------------------------------------------
# (2) Add body temperature: habitat difference (VIOLATES ASSUMPTIONS)
# ------------------------------------------------------------

lizard_dat <- lizard_dat %>%
  mutate(
    
    body_temp_C = ifelse(
      habitat == "Open",
      
      rgamma(n(), shape = 18, scale = 1.6),   # Higher mean, larger variance
      rgamma(n(), shape = 35, scale = 0.8)    # Lower variance, less skew
    )
  )

# ------------------------------------------------------------
# (3) Add speed: temperature relationship (VIOLATES ASSUMPTIONS)
# ------------------------------------------------------------

# Nonlinear thermal performance curve
thermal_curve <- function(temp) {
  3 + 0.9 * temp - 0.015 * temp^2
}

lizard_dat <- lizard_dat %>%
  mutate(
    
    true_speed = thermal_curve(body_temp_C),
    
    # Heteroskedastic + non-normal noise
    speed_m_s =
      true_speed +
      
      rt(n(), df = 3) * (0.2 + 0.02 * body_temp_C) +  # heavy tails + heteroskedasticity
      
      0.4 * sin(body_temp_C / 2)                       # Hidden nonlinearity
  )

# Add weak non-independence via pseudo repeated structure
lizard_dat <- lizard_dat %>%
  mutate(
    cluster = sample(1:12, n(), replace = TRUE),
    speed_m_s = speed_m_s + cluster * 0.03
  )

# Add a few obvious outliers
lizard_dat$speed_m_s[c(10, 88, 190)] <- lizard_dat$speed_m_s[c(10, 88, 190)] + c(3, -2.5, 2.8)

# Body size and sex (Assumptions met)
lizard_dat %>% ggplot(aes(y =body_size_mm, x = sex)) + geom_boxplot()
shapiro.test(lizard_dat[lizard_dat$sex == "Female",]$body_size_mm)
shapiro.test(lizard_dat[lizard_dat$sex == "Male",]$body_size_mm)
car::leveneTest(y = lizard_dat$body_size_mm, group = lizard_dat$sex)
summary(aov(body_size_mm ~ sex, data = lizard_dat))
summary(lm(body_size_mm ~ sex, data = lizard_dat))

# Body size and age (assumptions met)
lizard_dat %>% ggplot(aes(y =body_size_mm, x = age_years)) + geom_point() + geom_smooth(method = 'lm')
size_age_mod <- lm(body_size_mm ~ age_years, data = lizard_dat)
summary(size_age_mod)
plot(size_age_mod)

# Body temp and habitat (Assumptions NOT met)
lizard_dat %>% ggplot(aes(x =body_temp_C)) + facet_wrap(~habitat) + geom_density()
lizard_dat %>% ggplot(aes(y =body_temp_C, x = habitat)) + geom_boxplot()
shapiro.test(lizard_dat[lizard_dat$habitat == "Open",]$speed_m_s)
shapiro.test(lizard_dat[lizard_dat$habitat == "Forest",]$speed_m_s)
car::leveneTest(y = lizard_dat$body_temp_C, group = lizard_dat$habitat)
summary(aov(body_temp_C ~ habitat, data = lizard_dat))
summary(lm(body_temp_C ~ habitat, data = lizard_dat))

# Speed and body temp (assumptions NOT met)
lizard_dat %>% ggplot(aes(y =speed_m_s, x = body_temp_C)) + geom_point() + geom_smooth(method = 'lm')
temp_speed_mod <- lm(speed_m_s ~ body_temp_C, data = lizard_dat)
summary(temp_speed_mod)
plot(temp_speed_mod)

# Remove unnecessary columns
lizard_dat_save <- lizard_dat %>%
  select(! c(true_speed, cluster))

# Save the data
write.csv(lizard_dat_save, "datasets/lizard_data.csv", row.names = FALSE)
