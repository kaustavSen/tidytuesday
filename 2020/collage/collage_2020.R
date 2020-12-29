library(tidyverse)
library(magick)
library(ggimage)
library(ggforce)
library(showtext)
library(here)

# Create mask to "circulize" images ---------------------------------------

png(here("2020", "collage", "images", "mask.png"), 1000, 1000)
par(mar = rep(0,4), yaxs="i", xaxs="i")
plot(0, type = "n", ylim = c(0,1), xlim=c(0,1), axes=F, xlab=NA, ylab=NA)
plotrix::draw.circle(.5,0.5,.5, col="black")
dev.off()

circle_mask <- image_read(here(2020, "collage", "images", "mask.png")) %>% 
  image_scale("150")


# Get data ----------------------------------------------------------------

tt_data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/tidytuesday_tweets/data.csv", guess_max = 100000)


# Filter for 2020 ---------------------------------------------------------

tt_data_2020 <- tt_data %>% 
  filter(created_at >= lubridate::ymd(20200101)) %>% 
  count(screen_name, profile_image_url, sort = TRUE) %>% 
  filter(n > 10) %>% 
  mutate(
    profile_image_url = str_replace(profile_image_url, "_normal", "")
  )


# Function to save twitter profile image ----------------------------------

save_image <- function(screen_name, profile_image_url) {
  image <- try(image_read(profile_image_url))
  
  if(class(image) != "try-error") {
    image <- image_scale(image, "150")
    image_composite(circle_mask, image, "plus", gravity = "center") %>% 
      image_write(here("2020", "collage", "images", paste0(screen_name, ".png")))
  }
}

# Save images on disk -----------------------------------------------------

tt_data_2020 %>% 
  select(screen_name, profile_image_url) %>% 
  pwalk(~save_image(.x, .y))


# Create Plot ------------------------------------------------------------

plot_data <- tt_data_2020 %>% 
  mutate(
    image_loc = here("2020", "collage", "images", paste0(screen_name, ".png")),
    is_present = file.exists(image_loc),
    bucket = cut(n, breaks = c(-Inf, 20, 49, Inf), labels = 1:3)
  ) %>% 
  select(-n) %>% 
  filter(is_present, screen_name != "R4DScommunity") %>% 
  group_by(bucket) %>% 
  mutate(
    n = row_number(),
    tot = n()
  ) %>% 
  nest() %>% 
  pmap(
    ~mutate(
      .y, 
      angle = seq(0, 360 - 360/.y$tot[1], length.out = .y$tot[1]),
      x = cos(angle * pi / 180),
      y = sin(angle * pi / 180)
    )
  ) %>% 
  bind_rows(.id = "bucket") %>% 
  mutate(
    bucket = as.numeric(bucket),
    image_offset = bucket * 2.25
  )

main_image <- image_read("https://pbs.twimg.com/profile_images/985163908418711552/Y8gFf5Wc_400x400.jpg")
main_circle_mask <- image_read(here(2020, "collage", "images", "mask.png")) %>% 
  image_scale("400")

image_composite(main_circle_mask, main_image, "plus", gravity = "center") %>% 
  image_write(here("2020", "collage", "images", "main.png"))

main_logo_df <- tibble(x = 0, y = 0, image = main_logo)

font_add_google("Bungee Inline")
font_add_google("Unica One")
font_add_google("Roboto")

showtext_auto()

ggplot(plot_data) +
  geom_image(aes(x = 0, y = 0, image = here(2020, "collage", "images", "main.png")), size = 0.18) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1.55), size = 2.5, color = "#eba07e") +
  geom_image(aes(x = x * image_offset, y = y * image_offset, image = image_loc)) +
  geom_circle(aes(x0 = x * image_offset, y0 = y * image_offset, r = 0.45), size = 1.8, color = "#5d74a5") +
  coord_equal() +
  labs(
    title = "#TidyTuesday 2020",
    subtitle = "Participants with more than ten submissions over 2020",
    caption = "**Data:** Twitter | **Plot:** Kaustav Sen"
  ) +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(family = "Bungee Inline", hjust = 0.5, size = 95),
    plot.subtitle = element_text(family = "Unica One", hjust = 0.5, size = 35, color = "grey75", margin = margin(t = 20)),
    plot.caption = ggtext::element_markdown(family = "Roboto", size = 25, color = "grey60")
  ) +
  ggsave(here(2020, "collage", "plot.pdf"), width = 25, height = 25, device = cairo_pdf)

pdftools::pdf_convert(here(2020, "collage", "plot.pdf"), filenames = here(2020, "collage", "plot.png"), dpi = 72)
