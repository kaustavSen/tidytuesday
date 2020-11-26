library(tidyverse)

theme_set(theme_minimal())

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

covid_data %>% 
  filter(location == "India") %>% 
  select(date, total_cases, new_cases, total_deaths, new_deaths) %>% 
  ggplot(aes(date, total_cases)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_comma(scale = 1/1e6, suffix = "m"))

covid_data_in %>% 
  mutate(total_cases_mil = floor(total_cases / 1e6) * 1e6) %>% 
  group_by(total_cases_mil) %>% 
  filter(total_cases_mil %in% c(1e6, 2e6, 3e6, 4e6)) %>% 
  slice(1)
