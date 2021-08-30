library(tidyverse)
library(lubridate)
library(ggtext)

theme_set(theme_void(base_size = 12, base_family = "Chillax"))
colors <- c("M" = "#3d405b","F" = "#81b29a")

tues_data <- tidytuesdayR::tt_load("2021-08-24")
lemur_data <- tues_data$lemur_data

lemur_sex_prop <- 
  lemur_data %>% 
  select(sex, dob) %>% 
  mutate(year = year(dob)) %>% 
  filter(!is.na(dob), year >= 1980, sex != "ND") %>% 
  count(year, sex) %>% 
  group_by(year) %>% 
  mutate(prop_sex = n / sum(n))

p <- 
  ggplot(lemur_sex_prop, aes(year, prop_sex, fill = sex)) +
  geom_hline(yintercept = seq(0, 1, 0.25), color = "grey30", size = 0.6, linetype = "dashed") +
  geom_col(alpha = 0.7, width = 1, color = "black", size = 0.9, show.legend = FALSE) +
  geom_text(data = tibble(x = seq(1985, 2015, 10), y = 0.1, label = c("1980's", "90's", "00's", "2010's")), 
            aes(x = x, y = y, label = label), inherit.aes = FALSE, family = "Chillax", 
            fontface = "bold", color = "grey60", alpha = 0.8, size = 12) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs(
    title = "Proportion of lemur births",
    subtitle = "<span style='color: #3d405b'>**males**</span> | <span style='color: #81b29a'>**females**</span>",
    x = "", 
    y = "",
    caption = "**data:** kaggle | **plot:** kaustav sen"
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = rel(2.8), hjust = 0.5, 
                              face = "bold", margin = margin(b = 5), color = "grey40"),
    plot.subtitle = element_markdown(size = rel(1.6), hjust = 0.5,
                                     margin = margin(b = 10)),
    plot.caption = element_markdown(hjust = 0.5, color = "grey70", size = rel(0.95), margin = margin(10)),
    plot.background = element_rect(fill = "grey95", color = "grey95"),
    plot.margin = margin(10, 20, 10, 20),
    axis.text.y = element_text(margin = margin(r = 5))
  )

ggsave("2021/plots/week_35.png", p, width = 12, height = 6, dpi = 300)  
