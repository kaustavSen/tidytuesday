library(tidyverse) 
library(lubridate)
library(ggbeeswarm)
library(wesanderson)
library(ggtext)
library(rtweet)

# Load the data
tues_data <- tidytuesdayR::tt_load(2021, week = 25)
tweets <- tues_data$tweets

# Create the data for the title
label_data <- tibble(
  datetime = as_datetime(ymd(20210330)),
  label = "<span style='color: #4682b4'>#DuBoisChallenge</span> 2021: Twitter Activity<br>*<span style='font-size: 16px'><span style='color: #ffc0cb'>#TidyTuesday</span> was a definite catalyst</span>*"
)

# Plot the data
tweets %>% 
  filter(!is.na(content)) %>% 
  # Converted to lower-case so that we get both #tidytuesday as well as #TidyTuesday
  mutate(tt_post = str_detect(str_to_lower(content), "tidytuesday")) %>% 
  # Y-axis is just a dummy here. Set equal to 1 so that all data is classified into
  # a single group
  ggplot(aes(datetime, 1)) +
  # Create the "bee-swarm" plot
  geom_quasirandom(aes(size = like_count, fill = tt_post),
                   groupOnX = FALSE, alpha = 0.8, shape = 21,
                   stroke = 0.5, color = "#654321") +
  # Add the title description
  geom_richtext(data = label_data,
                aes(y = 1.3, label = label), family = "Source Serif Pro",
                size = 6, label.color = NA, fill = "#d2b48c", lineheight = 0.8,
                fontface = "bold", color = "grey30") +
  # Colors picked up from the style guide:
  # https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf
  scale_fill_manual(values = c("#4682b4", "#ffc0cb")) +
  # Trying out size-binned which was introduced in ggplot2 v3.3.0
  # But in the end choose not to describe the size feature (probably obvious??)
  scale_size_binned(breaks = c(25, 50, 75, 100), range = c(3, 8)) +
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(
    x = "",
    y = "",
    caption = "**Data:** #DuBoisChallenge tweets | **Plot:** Kaustav Sen"
  ) +
  theme_minimal(base_size = 14, base_family = "Poppins") +
  theme(
    plot.background = element_rect(fill = "#d2b48c", color = NA),
    plot.margin = margin(t = 10, r = 15),
    plot.caption.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, size = rel(0.7), 
                                    margin = margin(b = 5), color = "grey30"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey40", size = 0.3),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  )
ggsave(here::here(2021, "plots", "week_25.png"), width = 9, height = 4, dpi = 150)

# Using {rtweet} to directly post the tweet from R! 
# post_tweet(
#   status =
#   "#TidyTuesday Week 25: A beeswarm plot of #DuBoisChallenge 2021 Twitter Activity.\n
#   Would be interesting to see if we can beat the last time's peak!\n
#   #RStats",
#   media = here::here(2021, "plots", "week_25.png"),
#   media_alt_text =
#   "Chart Type: A beeswarm plot showing the twitter activity of the
#   DuBoisChallenge challenge.
#   Type of Data: Time-series data of tweets plotted as points with the
#   size of the points indicating the likes count of the tweet
#   Reason for including the chart: Brings out the skipe observed in Twitter
#   activity as a result of the TidyTuesday challenge from Week 8 of 2021
#   Link to data source: Tweet data based on the DuBoisChallenge hastag"
# )
