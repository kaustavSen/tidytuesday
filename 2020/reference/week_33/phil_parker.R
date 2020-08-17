# Credit: @PhilParker_IPPE (https://github.com/pdparker/TidyTuesday/blob/master/tt_week_33.R)

# Stock packages
library(tidytuesdayR)
library(tidyverse)
# library(here)
library(glue)
library(magick)
library(purrr)
library(tidylog)
library(lubridate)
library(patchwork)
library(fuzzyjoin)
library(ggtext)
library(broom)
library(forcats)
library(ggforce)
library(ggdist)
library(janitor)
library(ggridges)
library(grid)
library(tidytext)
library(ggwordcloud)
library(tvthemes)
library(cowplot)
#pacman::p_load(gapminder, emo, patchwork, cowplot)
#Load fonts
extrafont::font_import("~/Library/Fonts/",prompt = FALSE)
extrafont::fonts()
# import_theLastAirbender()

tuesdata <- tidytuesdayR::tt_load(2020,week = 33)

sentiment <- 
  get_sentiments("nrc") %>%
  filter(sentiment %in% c('joy', 'sadness'))

avatar_adjective <- 
  tuesdata$avatar %>%
  mutate(direction = str_extract_all(full_text, "\\[(.+)\\]", simplify = TRUE),
         direction = str_remove_all(direction,"[[:punct:]]")) %>%
  filter(direction != '') %>%
  unnest_tokens(word, direction) %>%
  left_join(parts_of_speech) %>%
  filter(pos == "Adjective")  %>%
  left_join(., sentiment)

# plot joy ####
p1 <- 
  avatar_adjective %>%
  filter(sentiment == 'joy', word != 'white') %>%
  count(word,sort=TRUE, name = 'freq') %>%
  top_n(10) %>%
  mutate(word = fct_reorder(word, freq)) %>%
  ggplot(aes(y=word,x=freq)) +
  geom_col(fill = '#a00000') +
  theme_avatar() +
  geom_text(aes(label=freq), vjust=0.5, hjust = 1,
            color="white", size=6, nudge_x = -2) +
  geom_text(aes(label=word, x = 0), vjust=0.5, hjust = 0,
            color="white", size=6, nudge_x = 0.5) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.05)) +
  labs(title = "Joyous Words",
       y = '', x = '')

# plot sadness ####
p2 <- 
  avatar_adjective %>%
  filter(sentiment == 'sadness', word != 'shot') %>%
  count(word,sort=TRUE, name = 'freq') %>%
  top_n(10) %>%
  mutate(word = fct_reorder(word, freq)) %>%
  ggplot(aes(y=word,x=freq)) +
  geom_col(fill = '#7d605e') +
  scale_x_reverse() +
  scale_y_discrete(position = 'right') +
  theme_avatar() +
  geom_text(aes(label=freq), vjust=0.5, hjust = 1,
            color="white", size=6, nudge_x = 2) +
  geom_text(aes(label=word, x = 0), vjust=0.5, hjust = 1,
            color="white", size=6, nudge_x = -0.5) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.95)) +
  labs(title = "Sad Words",
       y = '', x = '')

# plot line ####
p3 <- avatar_adjective %>%
  filter(!is.na(sentiment), word != 'shot' ) %>%
  mutate(book_num = glue("Book {book_num}")) %>%
  group_by(book_num, chapter_num) %>%
  count(sentiment, sort = TRUE) %>%
  ggplot(aes(x = chapter_num, y = n, color = sentiment, group = sentiment)) +
  geom_line(size = 1.5) + 
  theme_avatar() +
  scale_color_manual(values = c('#a00000','#7d605e')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        panel.spacing = unit(0.1, "cm"),
        plot.tag.position = c(0.05,0.35),
        plot.tag = element_text(size = 20, angle = 270),
        axis.text.x = element_text(size = 20),
        strip.text = element_text(size = 20),
        axis.text.y = element_blank()) +
  facet_wrap(~book_num, strip.position = "bottom") +
  labs(x = '', tag = 'Chapter:',y = '') +
  expand_limits(x = -2) 

# word cloud ####
avatar_count <-  avatar_adjective %>%
  filter(sentiment %in% c('sadness', 'joy'), word != 'shot') %>%
  count(word,sort=TRUE, name = 'freq')

# Trial Plot #### 
# Wanted to clip to Avatar image but didn't work
# set.seed(42)
p6 <- ggplot(avatar_count, aes(label = word, size = freq)) +
  geom_text_wordcloud_area(
    mask = png::readPNG(here('img','avatar.png')),
    rm_outside = TRUE,
    color = '#a00000'
  ) +
  #scale_size_area(max_size = 8) +
  theme_avatar(title.font = "Herculanum",
               text.font = "Herculanum",
               title.size = 10) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Herculanum"))

df <- data.frame()
avatar <- magick::image_read("https://cdn.dribbble.com/users/2314387/screenshots/11401644/aang-high-resolution_2x.png")
p4 <- 
  ggplot(df) + 
  geom_point() + 
  xlim(0, 100) + 
  ylim(0, 100) +
  annotation_raster(avatar, 
                    ymin = -Inf, 
                    ymax = Inf,
                    xmin = -Inf, 
                    xmax = Inf
  )  +
  theme_avatar() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank()) +
  labs(y = '', x = '')


layout <- '
AABBCC
AABBCC
AABBCC
DDDDDD
'

p5 <- p1 + p4 + p2 + p3 +
  plot_annotation(
    title = 'Joy and sadness in Avatar: The Last Airbender\n',
    subtitle = 'Stage directions adjectives\n',
    caption = '@philparker_IPPE | tidyTuesday week 33'
  ) + 
  plot_layout(design = layout) &
  theme(text = element_text(family = 'Arial', size = 20),
        panel.background = element_rect(fill = '#ece5d3'),
        plot.background = element_rect(fill = '#ece5d3'),
        plot.title = element_text(size = 40, hjust = 0.05),
        plot.subtitle = element_text(size = 26, hjust = 0.05),
        plot.margin=unit(c(1,0.5,1,0.5),"cm"),
  )

ggsave(plot = p5, filename = here('week33_p1.png'),
       dpi = 300, width = 20, height = 15) 



# ggsave(plot = p6, filename = here('img','week33_p2.png'),
#        dpi = 300, height = 8, width = 4) 


# Earth, Air, water, Fire ####
avatar_eawf <- tuesdata$avatar %>% 
  mutate(character_words = str_remove_all(character_words,'[[:punct:]]')) %>%
  unnest_tokens(word,character_words) %>%
  mutate(word = tolower(word)) %>%
  filter(word %in% c('earth', 'air', 'water', 'fire', 'wind')) %>%
  group_by(book_num, chapter_num) %>%
  count(word)

# Earth ####
earth <- avatar_eawf %>%
  filter(word == 'earth') %>%
  ggplot(aes(x = chapter_num, y = book_num, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = '#1aaa4b', high = '#015027') +
  theme_void() +
  labs(title = 'Earth', y = 'Book: ', x = 'Chapter: ') +
  theme(plot.title = element_text(size = 40, hjust = 0.05,
                                  family = 'Herculanum', color = 'white'),
        text = element_text(size = 20, family = 'Herculanum', color = 'white'),
        axis.title.x  = element_text(size = 20, family = 'Herculanum', 
                                     color = 'white', hjust = 1),
        axis.title.y = element_text(size = 20, family = 'Herculanum', color = 'white', hjust = 1, angle = 90),
        axis.text = element_text(size = 16, family = 'Herculanum', color = 'white'),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#262c3a'),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  panel_border(remove = TRUE)+
  coord_equal()

# fire ####
fire <- avatar_eawf %>%
  filter(word == 'fire') %>%
  ggplot(aes(x = chapter_num, y = book_num, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = '#f39713', high = '#ee1b25') +
  theme_void() +
  labs(title = 'Fire', y = 'Book: ', x = 'Chapter: ') +
  theme(plot.title = element_text(size = 40, hjust = 0.05,
                                  family = 'Herculanum', color = 'white'),
        text = element_text(size = 20, family = 'Herculanum', color = 'white'),
        axis.title.x  = element_text(size = 20, family = 'Herculanum', color = 'white', hjust = 1),
        axis.title.y = element_text(size = 20, family = 'Herculanum', color = 'white', hjust = 1, angle = 90),
        axis.text = element_text(size = 16, family = 'Herculanum', color = 'white'),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#262c3a'),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  panel_border(remove = TRUE) +
  coord_equal()

# air ####
air <- avatar_eawf %>%
  filter(word %in% c('air','wind') ) %>%
  ggplot(aes(x = chapter_num, y = book_num, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = '#fefac7', high = '#fef500') +
  theme_void() +
  labs(title = 'Air', y = 'Book: ', x = 'Chapter: ') +
  theme(plot.title = element_text(size = 40, hjust = 0.05,
                                  family = 'Herculanum', color = 'white'),
        text = element_text(size = 20, family = 'Herculanum', color = 'white'),
        axis.title.x  = element_text(size = 20, family = 'Herculanum', color = 'white', hjust = 1),
        axis.title.y = element_text(size = 20, family = 'Herculanum', color = 'white', hjust = 1, angle = 90),
        axis.text = element_text(size = 16, family = 'Herculanum', color = 'white'),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#262c3a'),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  panel_border(remove = TRUE) +
  coord_equal()

# water ####
water <- avatar_eawf %>%
  filter(word == 'water') %>%
  ggplot(aes(x = chapter_num, y = book_num, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = '#b7e5f2', high = '#00bef1') +
  theme_void() +
  labs(title = 'Water', y = 'Book: ', x = 'Chapter: ') +
  theme(plot.title = element_text(size = 40, hjust = 0.05,
                                  family = 'Herculanum', color = 'white'),
        text = element_text(size = 20, family = 'Herculanum', color = 'white'),
        axis.title.x  = element_text(size = 20, family = 'Herculanum', color = 'white', hjust = 1),
        axis.title.y = element_text(size = 20, family = 'Herculanum', color = 'white', hjust = 1, angle = 90),
        axis.text = element_text(size = 16, family = 'Herculanum', color = 'white'),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#262c3a'),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  panel_border(remove = TRUE)+
  coord_equal()

(earth + fire) / (air + water) + 
  plot_annotation(title = "Mentions of: <span style='color:#1aaa4b'>Earth</span>, <span style='color:#ee1b25'>Fire</span>, <span style='color:#fefac7'>Air</span>, and <span style='color:#00bef1'>Water</span>",
                  theme = theme(plot.title = element_markdown(lineheight = 1.1,color = 'white', family = 'Herculanum', size = 40))) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom',
        legend.spacing = unit(1.0, 'cm'),
        plot.background = element_rect(fill = '#262c3a',color='#262c3a'),
        panel.background = element_rect(fill = '#262c3a',color='#262c3a') )

ggsave(filename = here('img','week33_p3.png'),
       dpi = 300, width = 20, height = 8) 