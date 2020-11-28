# Source: https://github.com/gkaramanis/tidytuesday/blob/master/2020-week48/washington-hiking.R

library(tidyverse)

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

trails <- 
  hike_data %>% 
  mutate(name = str_trim(name)) %>% 
  separate(length, into = c("length", "roundtrip"), sep = ", ") %>% 
  mutate(
    length = parse_number(length) * 1609.34,  # miles to meters
    gain = parse_number(gain) * 0.3048, # feet to meters
    highpoint = parse_number(highpoint) * 0.3048, # feet to meters
    rating = as.numeric(rating),
    location = str_remove(location, "-- .+"),
    bin_length = as.numeric(cut(length, 5)), # bin length
    bin_highpoint = as.numeric(cut(highpoint, 5)) # bin highpoint
  )

length_plot <- 
  trails %>% 
  count(bin_length) %>% 
  mutate(
    freq_bin = bin_length / 5,
    freq_n = n / sum(n),
    freq_n_cumsum = cumsum(freq_n),
    freq_n_cumsum_lag = lag(freq_n_cumsum, default = 0)
  ) %>% 
  rowwise() %>% 
  mutate(
    x = list(c(freq_bin - 0.2, freq_bin - 0.2, freq_bin, freq_bin, freq_n_cumsum, freq_n_cumsum, freq_n_cumsum_lag, freq_n_cumsum_lag)),
    y = list(c(0.7, 0.8, 0.8, 0.7, -0.1, -0.2, -0.2, -0.1))
  ) %>% 
  unnest(c(x, y))

highpoint_plot <- 
  trails %>% 
  count(bin_highpoint) %>% 
  mutate(
    freq_bin = bin_highpoint / 5,
    freq_n = n / sum(n),
    freq_n_cumsum = cumsum(freq_n),
    freq_n_cumsum_lag = lag(cumsum(freq_n), default = 0),
  ) %>% 
  rowwise() %>% 
  mutate(
    x = list(1.25 + c(-0.1, -0.2, -0.2, -0.1,
                      0.7, 0.8, 0.8, 0.7)),
    y = list(0.9 + c(freq_bin - 0.2, freq_bin - 0.2, freq_bin, freq_bin,
                     freq_n_cumsum, freq_n_cumsum, freq_n_cumsum_lag, freq_n_cumsum_lag)),
  ) %>% 
  unnest(c(x, y))

wa_palette <- c("#008457", "#000000", "#FFD520", "#34C2DE", "#FDD6C6")

ggplot() +
  geom_polygon(
    data = length_plot,
    aes(
      x, y,
      group = bin_length,
      fill = as.factor(bin_length)
    ),
    color = "white", size = 1
  ) +
  geom_polygon(
    data = highpoint_plot,
    aes(
      x, y,
      group = bin_highpoint,
      fill = as.factor(bin_highpoint)
    ),
    color = "white", size = 1
  ) +
  scale_fill_manual(values = wa_palette) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  ggsave(here::here(2020, "reference", "week_48", "gkaramanis.pdf"), width = 9, height = 9, device = cairo_pdf)
