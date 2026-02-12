
# Scatterplot they will need to recreate

library(tidyverse)

# Body size vs. dispersal distance in insects

# Biological story: Larger insects tend to disperse farther. The strength of 
# this relationship varies among habitat types.


set.seed(42)

n <- 300

species_levels <- c("Dragonfly", "Grasshopper", "Beetle", "Butterfly")
habitat_levels <- c("Wetland", "Forest", "Grassland")  
# Intentionally awkward order for faceting exercise

region_levels <- c("North", "Central", "South", "Island")

dat <- tibble(
  species = sample(species_levels, n, replace = TRUE),
  habitat = sample(habitat_levels, n, replace = TRUE),
  region  = sample(region_levels, n, replace = TRUE)
)

# Body size distributions (mm)
dat <- dat %>%
  mutate(
    body_size_mm = rlnorm(n(),
                          meanlog = case_when(
                            species == "Dragonfly"  ~ 2.2,
                            species == "Grasshopper" ~ 2.0,
                            species == "Beetle"      ~ 1.8,
                            species == "Butterfly"   ~ 1.9
                          ),
                          sdlog = 0.25)
  )

# Habitat-dependent slopes
habitat_slope <- c(
  Forest = 2.5,
  Grassland = 3.2,
  Wetland = 2.8
)

# Species intercepts
species_intercept <- c(
  Dragonfly = 5,
  Grasshopper = 2,
  Beetle = 1,
  Butterfly = 3
)

dat <- dat %>%
  mutate(
    dispersal_distance_m =
      species_intercept[species] +
      habitat_slope[habitat] * body_size_mm +
      rnorm(n(), 0, 5)
  )

# Optional: make Island slightly weird
dat <- dat %>%
  mutate(
    dispersal_distance_m = ifelse(region == "Island",
                                  dispersal_distance_m * 0.6,
                                  dispersal_distance_m)
  )
