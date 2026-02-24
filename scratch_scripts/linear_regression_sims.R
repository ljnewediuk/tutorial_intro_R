
library(tidyverse)
library(faux)
library(ggpubr)

# Specify some human data
mean_age <- mean(4:20)
sd_age <- sd(4:20)
mean_height = 130
sd_height = 20

# Generate data
human_growth_data <- rnorm_multi(n = 200,
                                 mu = c(mean_age, mean_height),
                                 sd = c(sd_age, sd_height),
                                 r = 0.8,
                                 varnames = c('age', 'height_cm')) %>%
  rbind(data.frame(age = c(20, 5), height_cm = c(65, 190))) %>%
  filter(age > 0)

# Plot
ggscatter(human_growth_data,
          x = 'age',
          y = 'height_cm',
          add = 'reg.line')

