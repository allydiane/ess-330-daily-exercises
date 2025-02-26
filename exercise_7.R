#Ally Lewis
#2/25/25
#Daily Exercise 7

library(tidyverse)
library(dplyr)
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv' 
covid <- read_csv(url)
head(covid, 5)

#Question 1: The 6 states with the most current cases
top_states <- covid |>
  filter(date == max(date)) |>
  group_by(state) |>
  summarize(total_cases = sum(cases)) |>
  arrange(-total_cases) |>
  slice(1:6) |>
  pull(state)

#Filtering the raw data from above
filtered <- covid |>
  filter(state %in% top_states) |>
  group_by(date, state) |>
  summarize(total_cases = sum(cases), .groups = "drop")

#Set up a ggplot
plot <- ggplot(data=filtered, 
               aes(x = date, y = total_cases, color = state)) + 
  geom_line() + 
  labs(title = "States with the most current COVID Cases", 
      x = "Date", 
      y = "Cases", 
      color = "State", 
      size = "Cases") + 
  facet_wrap(~state, scales = "free_y") + 
  theme_minimal()

#save the image

#Question 2: 
covid_national <- covid |>
  group_by(date) |>
  summarize(daily_cases = sum(cases))

plot2 <- ggplot(covid_national, aes(x = date, y = daily_cases)) + 
  geom_col(fill = "steelblue") + 
  labs(title = "Daily COVID-19 cases in the USA", 
       x = "Date", 
       y = "Number of Cases") + 
  theme_minimal()

ggsave(plot2, filename = "img/nation.png")

plot2

