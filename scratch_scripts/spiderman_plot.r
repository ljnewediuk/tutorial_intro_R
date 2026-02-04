
# Graph popularity of "spiderman pointing meme" vs. Google searches for "who is alexa"

library(tidyverse)

# eyeballed data from graph
spiderman_data <- tibble(
  year = 2007:2023,
  spiderman_pop = c(0, 1, 2, 1.5, 1, 1, 0, 0, 0, 0, 2, 23, 37, 42, 42.5, 49, 42),
  alexa_search = c(6, 6, 10, 11, 13, 13, 13, 13, 19, 13, 27, 46, 48, 63, 64, 77, 48)
)

# plot
spiderman_data %>%
  ggplot(aes(x = alexa_search, y = spiderman_pop)) +
  geom_point(size = 3) +
  geom_smooth(method = 'lm', colour = 'red', fill = 'red') +
  theme(panel.background = element_rect(colour = 'black', fill = 'white', linewidth = 2),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        axis.title.x = element_text(size = 16, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 16, colour = 'black', vjust = 5),
        axis.text = element_text(size = 16, colour = 'black'),
        legend.text = element_text(size = 16, colour = 'black'),
        legend.title = element_text(size = 16, colour = 'black', vjust = 5),
        legend.key.height = unit(.6, 'cm'),
        legend.key.width = unit(.6, 'cm'),
        legend.position = "inside",
        legend.position.inside = c(.15,.5),
        legend.background = element_rect(colour = NA, fill = NA)) +
  labs(y = "Relative popularity of \n 'spiderman pointing' meme", 
       x = "Volume of Google searches for \n 'who is alexa'")

ggsave("imgs/spiderman_graph.svg", width = 5, height = 4, device = "svg")
