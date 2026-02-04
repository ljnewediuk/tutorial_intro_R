
# Load tidyverse
library(tidyverse)

# Load elephant data
dino_data <- read_table('datasets/dinosaurs.tsv')

# Get mammal sleep data
data("msleep")
# Log body weight
msleep$bodywt <- log(msleep$bodywt)

# Simulate some data
sim_dat <- data.frame(x = rnorm(20, mean = 5, sd = 3)) %>%
  mutate(a = -5 + x * 3 + rnorm(length(x), 0, 3),
         b = 20 + x * -2 + rnorm(length(x), 0, 2),
         c = x^1.5 + rnorm(length(x), 0, 5),
         d = 30 + x * - 4 + rnorm(length(x), 0, 3),
         e = x + rnorm(length(x), 0, 1))

# Bad and messy plot without proper labels
sim_dat %>%
  pivot_longer(cols = c(a, b, c, d, e)) %>%
  ggplot(aes(x = x, y = value)) +
  geom_point(aes(pch = name), size = 4) +
  geom_text(aes(label = name), vjust = 2) +
  theme(legend.position = "none") +
  labs(x = "", y = "y") +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 2),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 18, colour = 'black', vjust = 5),
        axis.text = element_text(size = 18, colour = 'black'),
        legend.position = "none") 

ggsave("imgs/bad_lake_plot.svg", width = 6, height = 5, device = "svg")

# Better plot
sim_dat %>%
  pivot_longer(cols = c(a, b, c, d, e)) %>%
  mutate(`Coastal distance` = factor(name, 
                                     levels = c('c', 'a', 'e', 'b', 'd'),
                                     labels = c('> 100 km', '80-99 km', '50-79 km', '30-49 km', '10-29 km'))) %>%
  ggplot(aes(x = x, y = value)) +
  geom_point(size = 0.75) +
  geom_smooth(aes(colour = `Coastal distance`, fill = `Coastal distance`), size = 1, method = 'lm') +
  scale_colour_brewer(palette = 'Blues', direction = -1) +
  scale_fill_brewer(palette = 'Blues', direction = -1) +
  theme(legend.position = "none") +
  labs(x = "Lake size (ha)", y = "Water turbidity (NTU)") +
  theme(panel.background = element_rect(colour = 'black', fill = 'darkgrey', linewidth = 2),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 18, colour = 'black', vjust = 5),
        axis.text = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black', vjust = 5),
        legend.key.height = unit(.6, 'cm'),
        legend.key.width = unit(.6, 'cm'),
        legend.position = "right",
        legend.background = element_rect(colour = NA, fill = NA)) 

ggsave("imgs/good_lake_plot.svg", width = 7, height = 5, device = "svg")

# Bad dino plot
dino_data %>%
  # pivot_longer(cols = c(a, b, c, d, e)) %>%
  ggplot(aes(x = age, y = mass)) +
  geom_point(aes(colour = species), size = 3) + 
  scale_colour_brewer(palette = 'Blues', direction = -1) +
  labs(x = 'Age (years)',
       y = 'Mass (g)') +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 2),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 18, colour = 'black', vjust = 5),
        axis.text = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black', vjust = 5),
        legend.key.height = unit(.6, 'cm'),
        legend.key.width = unit(.6, 'cm'),
        legend.background = element_rect(colour = NA, fill = NA))

ggsave("imgs/bad_dino_plot.svg", width = 8, height = 5, device = "svg")

# Good dino plot
dino_data %>%
  # pivot_longer(cols = c(a, b, c, d, e)) %>%
  ggplot(aes(x = age, y = log(mass))) +
  geom_point(aes(colour = species), size = 3) + 
  scale_colour_brewer(palette = 'Dark2') +
  labs(x = 'Age (years)',
       y = 'Log mass (g)') +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 2),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 18, colour = 'black', vjust = 5),
        axis.text = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black', vjust = 5),
        legend.key.height = unit(.6, 'cm'),
        legend.key.width = unit(.6, 'cm'),
        legend.background = element_rect(colour = NA, fill = NA)) 

ggsave("imgs/good_dino_plot.svg", width = 8, height = 5, device = "svg")

# Binned plot
msleep %>%
  ggplot(aes(x = bodywt, y = sleep_total, colour = order)) +
  geom_point(size = 3) +
  scale_colour_viridis_d() +
  theme(panel.background = element_rect(colour = 'black', fill = 'lightgrey', linewidth = 2),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 18, colour = 'black', vjust = 5),
        axis.text = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 18, colour = 'black', vjust = 5),
        legend.key.height = unit(.6, 'cm'),
        legend.key.width = unit(.6, 'cm'),
        legend.background = element_rect(colour = NA, fill = NA)) +
  labs(x = "Log body weight (Kg)", y = "Total sleep (hours)")

ggsave("imgs/good_binned_plot.svg", width = 8, height = 5, device = "svg")

msleep %>%
  mutate(wt_bin = case_when(bodywt < -2.5 ~ "Light",
                            bodywt >= -2.5 & bodywt < 0 ~ "Medium",
                            bodywt >= 0 & bodywt < 2.5 ~ "Heavy",
                            bodywt >= 2.5 ~ "Heaviest")) %>%
  ggplot(aes(x = wt_bin, y = sleep_total, fill = wt_bin)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(name = "Relative weight") +
  theme(panel.background = element_rect(colour = 'black', fill = 'lightgrey', linewidth = 2),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 18, colour = 'black', vjust = 5),
        axis.text = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 18, colour = 'black', vjust = 5),
        legend.key.height = unit(.6, 'cm'),
        legend.key.width = unit(.6, 'cm'),
        legend.background = element_rect(colour = NA, fill = NA)) +
  labs(x = "Log body weight (Kg)", y = "Total sleep (hours)")

ggsave("imgs/bad_binned_plot.svg", width = 8, height = 5, device = "svg")

