
library(tidyverse)

# 1 - Scatterplot data (cell size and ATP production)
# Explanatory: Cell size 
# Response: Metabolic rate
# Groups for faceting: Cell type (muscle, neuron, epithelial)

set.seed(1234)

cell_atp_data <- tibble(
  cell_type = c(rep('epithelial', 50), rep('neuron', 50), rep('muscle', 50)),
  length_um = round(c(rnorm(50, 36, 7), rnorm(50, 100, 30), rnorm(50, 30000, 10000)), 2)
) %>% 
  mutate(metabolic_rate_pmol_min = case_when(
    cell_type == 'epithelial' ~ (0.3 * log(length_um)),
    cell_type == 'neuron' ~ (0.8 * log(length_um)),
    cell_type == 'muscle' ~ (1.3 * log(length_um)))) %>%
  group_by(cell_type) %>%
  mutate(metabolic_rate_pmol_min = round(metabolic_rate_pmol_min + 
           rnorm(50, 0, metabolic_rate_pmol_min/4), 2))

# Save the data
write.csv(cell_atp_data, "datasets/cell_metabolism.csv", row.names = F)
  
# Relationship and clear groups, but muscle cells are much larger, which will
# force them to log the length
cell_atp_data %>%
  ggplot(aes(x = log(length_um), y = metabolic_rate_pmol_min, colour = cell_type)) +
  geom_point()

# 2 - Boxplot data (stress and cortisol)
# Explanatory: Treatment (control/heat stress/food restriction)
# Response: Circulating cortisol
# Groups for additional comparison: Sex

set.seed(5678)

cortisol_data <- expand.grid(
  treatment = c("Control", "Heat stress", "Diet restriction"),
  sex = c("F", "M"),
  rep(1:40)
)

stress_means <- c(Control = 5, `Heat stress` = 12, `Diet restriction` = 20)

sex_effect <- c(`F` = 1.6, M = 1)

cortisol_data$cortisol_ng_ml <- rlnorm(
  nrow(cortisol_data),
  meanlog = log(stress_means[cortisol_data$treatment] * sex_effect[cortisol_data$sex]),
  sdlog = 0.35
)

cortisol_data <- cortisol_data %>%
  select(!Var3) %>%
  mutate(cortisol_ng_ml = round(cortisol_ng_ml, 2))

# Save the data
write.csv(cortisol_data, "datasets/cortisol_stress.csv", row.names = F)

# Cort increases with heat stress and diet restriction, and is also higher for 
# females than males. They can plot them separately and then we can consider faceting.
cortisol_data %>%
  ggplot(aes(x = treatment, y = cortisol_ng_ml)) +
  geom_boxplot() +
  facet_wrap(~ sex)

# 3 - Line graph data (heart rate recovery after exercise)
# Explanatory: Time
# Response: Heart rate
# Groups for additional comparison: Fitness level

set.seed(91011)

# minutes post-exercise
time <- seq(0, 20, by = 1)  

fitness_levels <- c("Low", "Moderate", "High")

recovery_data <- lapply(fitness_levels, function(fit) {
  
  resting_hr <- switch(fit,
                       "Low" = 75,
                       "Moderate" = 65,
                       "High" = 55)
  
  recovery_rate <- switch(fit,
                          "Low" = 0.08,
                          "Moderate" = 0.12,
                          "High" = 0.18)
  
  peak_hr <- 170
  
  heart_rate <- resting_hr +
    (peak_hr - resting_hr) * exp(-recovery_rate * time) +
    rnorm(length(time), 0, 3)
  
  data.frame(
    time_min = time,
    heart_rate_bpm = heart_rate,
    fitness_level = fit
  )
})

recovery_data <- do.call(rbind, recovery_data)

recovery_data$heart_rate_bpm <- round(recovery_data$heart_rate_bpm, 0)

# Save the data
write.csv(recovery_data, "datasets/exercise_recovery.csv", row.names = F)

# Plot
recovery_data %>%
  ggplot(aes(x = time_min, y = heart_rate_bpm, group = fitness_level)) + 
  geom_line()

# 4 - Bar chart data (survival counts across fish stocking treatments)
# Explanatory: Stocking density
# Response: Proportion surviving

set.seed(101112)

densities <- c("Low", "Medium", "High", "Very high")
environments <- c("Artificial tank", "Natural pond")

fish_data <- expand.grid(
  density = densities,
  environment = environments
)

# Number stocked per tank
fish_data$n_stocked <- 100

# Survival probabilities
survival_prob <- with(
  fish_data, ifelse(environment == "Artificial tank",
                    c(Low = 0.85, Medium = 0.65, High = 0.40, `Very high` = 0.15)[density],
                    c(Low = 0.90, Medium = 0.85, High = 0.70, `Very high` = 0.55)[density])
)

fish_data$n_survived <- rbinom(
  nrow(fish_data),
  size = fish_data$n_stocked,
  prob = survival_prob
)

fish_data$survival_proportion <- fish_data$n_survived / fish_data$n_stocked

# Save the data
write.csv(fish_data, "datasets/fish_stocking.csv", row.names = F)

# Plot
fish_data %>%
  ggplot(aes(x = density, y = survival_proportion)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ environment)

