
# load tidyverse
library(tidyverse)

# Load the data 
crimes <- read_csv("datasets/crime_rates.csv")

# Arrange dataset
crimes_clean <- crimes %>%
  mutate(crime_per100K_US = violent_crime_per100K_US + property_crime_per100K_US,
         crime_per100K_CA = violent_crime_per100K_CA + property_crime_per_100K_CA) %>%
  pivot_longer(cols = c(crime_per100K_US, crime_per100K_CA),
               names_prefix = "crime_per100K_",
               values_to = "crime_per100K",
               names_to = 'country')

# plot full range
crimes_clean %>%
ggplot(aes(x = year, y = crime_per100K, colour = country)) +
  geom_line(linewidth = 1) +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 2),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 18, colour = 'black', vjust = 5),
        axis.text = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 18, colour = 'black'),
        legend.title = element_text(size = 18, colour = 'black', vjust = 5),
        legend.key.height = unit(.6, 'cm'),
        legend.key.width = unit(.6, 'cm'),
        legend.position = "inside",
        legend.position.inside = c(.15,.5),
        legend.background = element_rect(colour = NA, fill = NA)) +
  labs(y = "Crime rate (crimes/100K)", x = "Year")

ggsave("imgs/crime_rates_full.svg", width = 5, height = 4, device = "svg")

# Plot cherry-picked data
crimes_clean %>%
ggplot(aes(x = year, y = crime_per100K, colour = country)) +
  geom_line(linewidth = 1) +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 2),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 18, colour = 'black', vjust = 5),
        axis.text = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 18, colour = 'black'),
        legend.title = element_text(size = 18, colour = 'black', vjust = 5),
        legend.key.height = unit(.6, 'cm'),
        legend.key.width = unit(.6, 'cm'),
        legend.position = "inside",
        legend.position.inside = c(.15,.5),
        legend.background = element_rect(colour = NA, fill = NA)) +
  labs(y = "Crime rate (crimes/100K)", x = "Year") +
  coord_cartesian(xlim = c(2020, 2023), ylim = c(2100, 2350))
  
ggsave("imgs/crime_rates_cherry.svg", width = 5, height = 4, device = "svg")
