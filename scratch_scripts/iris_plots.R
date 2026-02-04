
library(tidyverse)

data(iris)

iris %>%
  ggplot(aes(x = Petal.Length, fill = Species)) +
  scale_fill_manual(values = c('#ffbb7a', '#76e0e0', '#ee4866')) +
  geom_histogram(colour = '#E7E6E6') +
  theme(plot.background = element_rect(colour = NA, fill = '#353F4F'),
        panel.background = element_rect(colour = NA, fill = '#353F4F'),
        axis.line = element_line(colour = '#E7E6E6', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = '#353F4F'),
        legend.text = element_text(size = 12, colour = '#E7E6E6'),
        legend.title = element_text(size = 12, colour = '#E7E6E6'),
        axis.ticks = element_line(colour = '#E7E6E6', linewidth = 1),
        axis.text = element_text(size = 12, colour = '#E7E6E6'),
        axis.title = element_text(size = 12, colour = '#E7E6E6'))

iris %>%
  ggplot(aes(x = Species, y = Sepal.Length, fill = Species)) +
  scale_fill_manual(values = c('#ffbb7a', '#76e0e0', '#ee4866')) +
  geom_boxplot(colour = '#E7E6E6') +
    theme(plot.background = element_rect(colour = NA, fill = '#353F4F'),
          panel.background = element_rect(colour = NA, fill = '#353F4F'),
          axis.line = element_line(colour = '#E7E6E6', linewidth = 1),
          panel.grid = element_blank(),
          legend.background = element_rect(colour = NA, fill = '#353F4F'),
          legend.text = element_text(size = 12, colour = '#E7E6E6'),
          legend.title = element_text(size = 12, colour = '#E7E6E6'),
          axis.ticks = element_line(colour = '#E7E6E6', linewidth = 1),
          axis.text = element_text(size = 12, colour = '#E7E6E6'),
          axis.title = element_text(size = 12, colour = '#E7E6E6'))

iris %>%
  ggplot(aes(x = Petal.Width, y = Petal.Length)) +
  scale_colour_manual(values = c('#ffbb7a', '#76e0e0', '#ee4866')) +
  geom_point(aes(colour = Species)) +
  geom_smooth(method = 'lm', se = F, colour ='#E7E6E6', linewidth = 2) +
  theme(plot.background = element_rect(colour = NA, fill = '#353F4F'),
        panel.background = element_rect(colour = NA, fill = '#353F4F'),
        axis.line = element_line(colour = '#E7E6E6', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = '#353F4F'),
        legend.text = element_text(size = 12, colour = '#E7E6E6'),
        legend.title = element_text(size = 12, colour = '#E7E6E6'),
        axis.ticks = element_line(colour = '#E7E6E6', linewidth = 1),
        axis.text = element_text(size = 12, colour = '#E7E6E6'),
        axis.title = element_text(size = 12, colour = '#E7E6E6'))
