# -----------------------------------
# AFC 2026 Annual Survey Analysis — setup
# sourced by 01-demographics.R, 02-rates.R, 03-projects.R
# -----------------------------------

setwd("/Users/jocelyn/Documents/Pratt/Projects/afc-survey-2026")

# ------ libraries

library(dplyr) # for data management
library(tidyverse) # for data tidying
library(ggplot2) # for data visualization
library(grid) # for arrow() / unit() in geom_segment arrows
library(svglite) # for saving svgs
library(circlize) # for chord diagrams

# load data (survey responses were cleaned up for analysis and re-exported as a csv)
# fields with no responses were populated with "9999"
data <- read.csv("data/2026_responses.csv")
View(data)

# ------ total number of responses: 54
# made it a variable to calculate percentages
n_responses <- nrow(data)
n_responses

# number of respondents for each year of the survey so far
respondents_by_year <- tibble(
  year = 2018:2026,
  n_respondents = c(85, NA, 56, NA, 74, NA, 94, 120, 54)
)

# chart: number of respondents by year
respondents_by_year_plot <- ggplot(
  respondents_by_year,
  aes(x = year, y = n_respondents)
) +
  geom_col(fill = "steelblue", na.rm = TRUE) +
  geom_point(color = "steelblue", na.rm = TRUE) +
  scale_x_continuous(breaks = respondents_by_year$year) +
  labs(
    title = "Number of respondents by year",
    x = NULL,
    y = "# respondents"
  ) +
  theme_minimal()

respondents_by_year_plot

ggsave(
  "images/respondents_by_year.svg",
  plot = respondents_by_year_plot,
  width = 6,
  height = 4
)
