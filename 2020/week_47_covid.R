library(tidyverse)
library(ggforce)
library(showtext)

font_add_google("Alegreya Sans")
font_add_google("Alegreya")
font_add_google("Alegreya Sans SC")

covid_data <- 
  read_csv(
    file = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
    col_types = cols(
      iso_code = col_character(),
      continent = col_character(),
      location = col_character(),
      tests_units = col_character(),
      date = col_date(format = ""),
      .default = col_double()
    )
  )

covid_data_in <- 
  covid_data %>% 
  filter(location == "India") %>% 
  select(date, total_cases, new_cases, total_deaths, new_deaths)

df <- 
  covid_data_in %>% 
  mutate(total_cases_mil = floor(total_cases / 1e6) * 1e6) %>%
  group_by(total_cases_mil) %>% 
  filter(total_cases == 1 | total_cases_mil %in% c(2e6, 4e6, 6e6, 8e6)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  replace_na(list(total_deaths = 0)) %>% 
  mutate(
    days= as.numeric(date - lag(date)),
    deaths = total_deaths - lag(total_deaths, default = 0)
  ) %>% 
  select(total_cases_mil, days, deaths)

temp_df <- 
  df %>%
  replace_na(list(days = 0)) %>% 
  mutate(
    group = 1:5,
    days = days * 8e6/sum(days),
    days_cum = cumsum(days),
    days_lag = lag(days_cum), 
    deaths = deaths * 8e6/sum(deaths),
    deaths_cum = cumsum(deaths),
    deaths_lag = lag(deaths_cum) 
  ) %>% 
  drop_na() %>% 
  rowwise() %>% 
  mutate(
    x1 = list(c(0, 0, 1e6, 1e6)),
    y1 = list(c(total_cases_mil - 2e6, total_cases_mil, total_cases_mil, total_cases_mil - 2e6)),
    x2 = list(c(2.0e6, 2.0e6, 3.0e6, 3.0e6)),
    y2 = list(c(days_lag, days_cum, days_cum, days_lag)),
    x3 = list(c(4.0e6, 4.0e6, 5.0e6, 5.0e6)),
    y3 = list(c(deaths_lag, deaths_cum, deaths_cum, deaths_lag))
  ) %>% 
  unnest(c(x1, y1, x2, y2, x3, y3))

text_labels <- 
  df %>%
  replace_na(list(days = 0)) %>% 
  mutate(
    group = 1:5,
    days = days * 8e6/sum(days),
    days_cum = cumsum(days),
    days_lag = lag(days_cum), 
    deaths = deaths * 8e6/sum(deaths),
    deaths_cum = cumsum(deaths),
    deaths_lag = lag(deaths_cum) 
  ) %>% 
  drop_na() %>% 
  mutate(
    days_label = days_cum - days/2,
    deaths_label = deaths_cum - deaths/2
  ) %>% 
  select(days_label, deaths_label)

link1 <- 
  temp_df %>% 
  select(link_x = x1, link_y = y1) %>% 
  mutate(id = row_number()) %>% 
  filter(id %in% c(seq(3, 16, by = 4), seq(4, 16, by = 4)))

link2 <- 
  temp_df %>% 
  select(link_x = x2, link_y = y2) %>% 
  mutate(id = row_number()) %>% 
  filter(id %in% c(seq(1, 16, by = 4), seq(2, 16, by = 4)))

link3 <- 
  temp_df %>% 
  select(link2_x = x2, link2_y = y2) %>% 
  mutate(id = row_number()) %>% 
  filter(id %in% c(seq(3, 16, by = 4), seq(4, 16, by = 4)))

link4 <- 
  temp_df %>% 
  select(link2_x = x3, link2_y = y3) %>% 
  mutate(id = row_number()) %>% 
  filter(id %in% c(seq(1, 16, by = 4), seq(2, 16, by = 4)))

x <- 
  bind_rows(link1, link2) %>% 
  arrange(id) %>% 
  select(-id)

y <- 
  bind_rows(link3, link4) %>% 
  arrange(id) %>% 
  select(-id)

showtext_auto()

plot <- 
  bind_cols(temp_df, x, y) %>% 
  ggplot(aes(fill = as.factor(group))) +
  geom_shape(aes(x1, y1, group = group), 
             alpha = 0.6, 
             color = "white",
             size = 0.75,
             radius = unit(0, "cm")) +
  geom_shape(aes(link_x, link_y, group = group), 
             alpha = 0.45, 
             color = "white",
             size = 0.75,
             radius = unit(0, "cm")) +
  geom_shape(aes(x2, y2, group = group), 
             alpha = 0.6, 
             color = "white", 
             size = 0.75,
             radius = unit(0, "cm")) +
  geom_shape(aes(link2_x, link2_y, group = group), 
             alpha = 0.45, 
             color = "white",
             size = 0.75,
             radius = unit(0, "cm")) +
  geom_shape(aes(x3, y3, group = group), 
             alpha = 0.6, 
             color = "white", 
             size = 0.75,
             radius = unit(0, "cm")) +
  annotate("text", x = 0, y = 8.5e6, label = "Spread of COVID-19 in India", family = "Alegreya", fontface = "bold", hjust = 0, size = 12) +
  annotate("text", x = 0.5e6, y = -2e5, label = "Cases", family = "Alegreya Sans SC", fontface = "bold", color = "grey45", size =8.5) +
  annotate("text", x = 2.5e6, y = -2e5, label = "Days Taken", family = "Alegreya Sans SC", fontface = "bold", color = "grey45", size =8.5) +
  annotate("text", x = 4.5e6, y = -2e5, label = "Deaths", family = "Alegreya Sans SC", fontface = "bold", color = "grey45", size =8.5) +
  annotate("text", x = 0.5e6, y = seq(1e6, 8e6, 2e6), label = c("1 to\n2 Million", "2 Million-\n4 Million", "4 Million-\n6 Million", "6 Million-\n8 Million"), family = "Alegreya Sans", color = "grey10", size = 7) +
  annotate("text", x = 2.5e6, y = text_labels$days_label, label = c("190", "29", "23", "31"), family = "Alegreya Sans", color = "grey10", size = 8) +
  annotate("text", x = 4.5e6, y = text_labels$deaths_label, label = c("42k", "28k", "26k", "25k"), family = "Alegreya Sans", color = "grey10", size = 8) +
  theme_void() +
  guides(fill = "none") +
  labs(
    caption = "Data: Our World in Data | Plot: Kaustav Sen"
  ) +
  scale_fill_manual(values = c("#800000FF", "#FFA319FF", "#155F83FF", "#C16622FF")) +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_text(family = "Alegreya Sans", size = 12, color = "grey", margin = margin(b = 0, r = 0))
  )

path <- here::here(2020, "plots", "week_47.")

ggsave(paste0(path, "pdf"), plot, device = cairo_pdf, height = 9, width = 9)
pdftools::pdf_convert(paste0(path, "pdf"), filenames = paste0(path, "png"), dpi = 320)
