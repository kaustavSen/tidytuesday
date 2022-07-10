# load the packages
library(tidyverse)
library(systemfonts)
library(colorspace)

# get the data
rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

# setup the fonts and colors to use
register_font(
  name = "heading_font",
  plain = "/home/kaustav/.fonts/CabinetGrotesk-Extrabold.otf"
)

colors <- wesanderson::wes_palettes$Zissou1

# data munging
rent_sf_2012 <- rent |> 
  filter(year == 2012, city == "san francisco", beds == 1, baths == 1)

top_10_nhoods <- rent_sf_2012 |> 
  count(nhood, sort = TRUE) |> 
  head(10) |> 
  pull(nhood)

plot_data <-
  rent_sf_2012 |> 
  filter(nhood %in% top_10_nhoods) |> 
  group_by(nhood) |> 
  summarise(
    max_price = max(price),
    q1 = quantile(price, 0.25),
    q3 = quantile(price, 0.75)
  ) |> 
  mutate(nhood = fct_reorder(nhood, max_price)) |> 
  arrange(desc(nhood)) |> 
  mutate(
    y_mid = 10:1,
    y_start = y_mid - 0.30,
    y_end = y_mid + 0.30
  )

labels <- c("Pacific Heights", "Marina", "South Beach", "North Beach", "Russian Hill",
            "Nob Hill", "San Francisco", "Inner Sunset", "Outer Sunset", "Inner Richmond")

# plotting
plot <- 
  ggplot(plot_data) +
  geom_col(aes(max_price, nhood), width = 0.4, fill = colors[2]) +
  geom_rect(aes(xmin = q1, xmax = q3, ymin = y_start, ymax = y_end),
            fill = colors[1], alpha = 0.7) +
  geom_rect(aes(xmin = q1, xmax = q1 - 30, ymin = y_start, ymax = y_end),
            fill = darken(colors[1], 0.1)) +
  geom_rect(aes(xmin = q3, xmax = q3 + 30, ymin = y_start, ymax = y_end),
            fill = darken(colors[1], 0.1)) +
  geom_text(aes(x = q3 + 30, y = y_mid, label = "▶"), size = 4, 
            color = darken(colors[1], 0.1), vjust = 0.5, hjust = 0) +
  geom_text(aes(x = q1 - 30, y = y_mid, label = "◀"), size = 4, 
            color = darken(colors[1], 0.1), vjust = 0.5, hjust = 1) +
  geom_text(aes(x = 20, y = y_end + 0.05, label = labels), family = "heading_font", color = "grey45", size = 5.5, hjust = 0) +
  annotate("text", x = 3500, y = 10, label = str_wrap("Price range covering middle 50% of all quotes", 30),
           family = "abinet Grotesk", color = "white", size = 5, lineheight = 1.1) +
  annotate("text", x = 2600, y = 1, label = "Based on 2012 rental prices from Craigslist Rental Housing Posts",
           family = "Cabinet Grotesk", color = "grey40", size = 4.7, lineheight = 1.1, hjust = 0) +
  scale_x_continuous(labels = scales::comma_format(scale = 1/1000, suffix = "k")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Planning to move to the San Francisco bay area?",
    subtitle = str_wrap("A neighbourhood wise split of the maximum rental prices for a 1 BHK together with a representative price range", 90),
    caption = "Data: Pennington, Kate (2018). Bay Area Craigslist Rental Housing Posts, 2000-2018 | Plot: Kaustav Sen"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    plot.margin = margin(20, 30, 10, 30),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = "heading_font", size = rel(2), color = "grey45"),
    plot.subtitle = element_text(family = "Cabinet Grotesk", size = rel(1.1), color = "grey55",
                                 margin = margin(b = 15)),
    plot.caption = element_text(family = "Cabinet Grotesk", size = rel(0.9), color = "grey50",
                                hjust = 0.5, margin = margin(t = 30, b = 10)),
    axis.text.x = element_text(family = "JetBrains Mono", face = "bold", size = rel(1.4), color = "grey60"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(linetype = "longdash"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )
ggsave("2022/plots/week_27.png", plot = plot, width = 12, height = 12, dpi = 180)
