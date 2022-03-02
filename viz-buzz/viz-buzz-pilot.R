library(tidyverse)
library(magick)
theme_set(theme_light())

# This is just to look at it as we work
img <- image_read("~/Downloads/rage_quit_w_inset_and_cancelo.png") %>%
  image_resize(geometry = geometry_area(height = 400, width = 400))

fouls <- read_csv("~/Downloads/rage_quit_fouls.csv") %>%
  unite(player_season, player, season, remove = FALSE)

fouls %>%
  count(n_rage_fouls)

# Return to the text: not sure why some show and some don't
text <- fouls %>%
  filter(n_rage_fouls >= 4) %>%
  filter(player_season %in% c("Jordan Ayew_2020",
                              "Joao Cancelo_2021",
                              "Emmanuel Dennis_2021",
                              "N'Golo Kante_2019",
                              "Benjamin Mendy_2020")) %>%
  separate(player, c("first", "last"), sep = " ") %>%
  mutate(player_year = paste0(last, " '", season %% 100))

# Add the y-labels

# Get a curve around to each

library(ggrepel)

y_data <- tibble(x = 100,
                 y = 1:5,
                 lab = c(seq(1, 4), '5 "rage" fouls'))

fouls %>%
  crossing(x_multiplier = seq(0, .9, .05)) %>%
  mutate(x = total_minutes_played  * x_multiplier) %>%
  mutate(player = fct_reorder(player, n_rage_fouls, max)) %>%
  ggplot(aes(x, n_rage_fouls * x_multiplier)) +
  geom_line(aes(group = player_season,
                color = factor(n_rage_fouls))) +
  coord_polar() +
  geom_text(aes(x, y, label = lab), data = y_data,
            color = "white",
            hjust = 0) +
  geom_text_repel(aes(x = total_minutes_played,
                y = n_rage_fouls,
                label = player_year),
            color = "white",
            data = text) +
  expand_limits(x = max(fouls$total_minutes_played),
                y = 0) +
  scale_color_manual(values = c("purple", "orange", "cyan", "darkred", "white")) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(color = "white")) +
  labs(x = "", y = "",
       title = 'Who "rage" fouls most frequently?',
       subtitle = 'Cancelo has commited the most "rage" fouls (4)\nin the 2021/22 Premier League season.',
       caption = '"Rage" foul: a foul within 7 seconds of own turnover\n\nViz: Tony ElHabr + David Robinson')
  
ggsave("~/Desktop/vizbuzz.jpg")
  