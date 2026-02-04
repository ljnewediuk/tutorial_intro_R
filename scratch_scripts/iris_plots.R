
library(tidyverse)

data(iris)
data(Nile)
data(fish_encounters)

# Histogram
iris %>%
  ggplot(aes(x = Petal.Length, fill = Species)) +
  scale_fill_manual(values = c('#ffbb7a', '#76e0e0', '#ee4866')) +
  geom_histogram(colour = 'black') +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black'))

# Boxplot
iris %>%
  ggplot(aes(x = Species, y = Sepal.Length, fill = Species)) +
  scale_fill_manual(values = c('#ffbb7a', '#76e0e0', '#ee4866')) +
  geom_boxplot(colour = 'black', linewidth = 1) +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(y = 'Sepal length', x = 'Species')

ggsave("imgs/example_boxplot.svg", width = 6, height = 5, device = "svg")

# Scatterplot
iris %>%
  ggplot(aes(x = Petal.Width, y = Petal.Length)) +
  scale_colour_manual(values = c('#ffbb7a', '#76e0e0', '#ee4866')) +
  geom_point(aes(colour = Species), size = 2) +
  geom_smooth(method = 'lm', se = F, colour ='black', linewidth = 2) +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(y = 'Petal width', x = 'Petal length')

ggsave("imgs/example_scatter.svg", width = 6, height = 5, device = "svg")

# Line graph
tibble(Stream.Flow = Nile, Year = 1871:1970) %>%
  ggplot(aes(x = Year, y = Stream.Flow)) +
  geom_line(linewidth = 1, colour = 'black') +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text = element_text(size = 14, colour = 'black'),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(y = 'Stream flow rate', x = 'Year')

ggsave("imgs/example_line.svg", width = 6, height = 5, device = "svg")
  
# Bar graph
fish_encounters %>%
  rename(Monitoring.Station = station) %>%
  group_by(Monitoring.Station) %>%
  summarize(Fish.Observed = sum(seen)) %>%
  ggplot(aes(x = Monitoring.Station, y = Fish.Observed)) +
  geom_bar(stat = "identity") +
  theme(plot.background = element_rect(colour = 'black', fill = 'white'),
        panel.background = element_rect(colour = NA, fill = 'white'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = NA, fill = 'white'),
        legend.text = element_text(size = 14, colour = 'black'),
        legend.title = element_text(size = 14, colour = 'black'),
        axis.ticks = element_line(colour = 'black', linewidth = 1),
        axis.text.y = element_text(size = 14, colour = 'black'),
        axis.text.x = element_text(size = 14, colour = 'black', angle = 60, vjust = 0.5),
        axis.title = element_text(size = 14, colour = 'black')) +
  labs(y = 'Number of fish observed', x = 'Monitoring station')

ggsave("imgs/example_barchart.svg", width = 6, height = 5, device = "svg")
