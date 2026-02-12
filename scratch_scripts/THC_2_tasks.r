
# Scatterplot they will need to recreate

library(tidyverse)

# Load data
insect_dat <- read_csv("datasets/insect_dispersal.csv") %>%
  filter(region != "I") %>%
  mutate(
    species = factor(
      species, 
      levels = c("cicindela_patruela", "melanoplus_femurrubrum", "anax_walsinghami" , "limenitis_arthemis"),
      labels = c("C. patruela", "M. femurrubrum", "A. walsinghami", "L. arthemis")),
    region = factor(
      region, 
      levels = c("N", "S", "C"), 
      labels = c("North", "South", "Central")
    ))

insect_dat %>%
  ggplot(aes(x = body_size_mm, y = dispersal_distance_m)) +
  geom_point(aes(colour = species)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ region) +
  labs(x = "Body size (mm)", y = "Dispersal distance (m)")

ggsave("TCH2_plot.tiff", last_plot(), path = "imgs/", device = "tiff",
       width = 10, height = 6, units = "in")

