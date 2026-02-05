
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
  mutate(metabolic_rate_pmol_min = metabolic_rate_pmol_min + 
           rnorm(50, 0, metabolic_rate_pmol_min/4))
  
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

cortisol_data <- select(cortisol_data, !Var3)

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

# 4 - Bar chart data (survival counts across fish stocking treatments)
# Explanatory: Stocking density
# Response: Proportion surviving

