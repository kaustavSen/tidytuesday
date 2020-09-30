library(tidyverse)
library(ggimage)
library(cowplot)
library(tidytuesdayR)
library(showtext)
library(ggtext)
library(here)

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')

font_add_google("Montserrat")
font_add_google("Playfair Display")

charts %>% 
  filter(chart == "US", artist == "Taylor Swift") %>% 
  mutate(released = str_extract(released, "[:alpha:]+ [:digit:]+, [:digit:]{4}"),
         released = lubridate::mdy(released)) %>%
  arrange(released) %>% 
  View()
  

charts %>% 
  filter(chart == "US", artist == "Beyoncé") %>% 
  mutate(released = str_extract(released, "[:alpha:]+ [:digit:]+, [:digit:]{4}"),
         released = lubridate::mdy(released)) %>% 
  arrange(released) %>% 
  View()

charts %>% 
  filter(artist == "Beyoncé") %>% 
  mutate(released = str_extract(released, "[:alpha:]+ [:digit:]+, [:digit:]{4}"),
         released = lubridate::mdy(released))

# color pallette: https://www.color-hex.com/color-palette/813


imgs <- 
  tibble(
  artist = c("taylor swift", "beyonce"),
  image_link = c(here("2020", "img", "taylor_swift.png"), here("2020", "img", "beyonce.png")),
  desc = c("**Taylor Swift** debuted with her eponymously titled album in 2006, which peaked at #5 in the US charts. Till date she has released 8 studio albums, 7 of which have topped the US charts.")
  ) 


showtext_auto()

p1 <- ggplot(imgs %>% filter(artist == "taylor swift")) +
  geom_image(aes(x = 0.615, y = 10, image = image_link), size = 0.1) +
  geom_textbox(aes(x = 0.6, y = 8.75, label = desc), size = 8, hjust = 0, 
               family = "Montserrat", color = "grey40", width = unit(15, "cm"), 
               box.color = NA, fill = NA) +
  scale_y_continuous(limits = c(-10, 10.5)) +
  scale_x_continuous(limits = c(0.6, 0.7)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#000000", color = "#000000")
  )

p2 <- ggplot(imgs %>% select(beyonce)) +
  geom_image(aes(x = 0.64, y = 10, image = beyonce), size = 0.25) +
  scale_y_continuous(limits = c(-10, 13)) +
  scale_x_continuous(limits = c(0.595, 0.65)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#000000", color = "#000000")
  )

plot_grid(p1, align = "h") +
  ggsave("temp.pdf", height = 25, width = 20, device = cairo_pdf)
