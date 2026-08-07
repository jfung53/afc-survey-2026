# -----------------------------------
# AFC 2026 Annual Survey Analysis
# -----------------------------------

setwd("/Users/jocelyn/Documents/Pratt/Projects/afc-survey-2026")

# ------ libraries

library(dplyr) # for data management
library(tidyverse) # for data tidying
library(ggplot2) # for data visualization

# load data (survey responses were cleaned up for analysis and re-exported as a csv)
# fields with no responses were populated with "9999"
data <- read.csv("data/2026_responses.csv")
View(data)

# ------ total number of responses: 54
# made it a variable to calculate percentages
n_responses <- nrow(data)
n_responses


# -----------------------------------
# --- DEMOGRAPHICS
# -----------------------------------

# --- GENDER ------------------------

table(data$gender)
# 9999        Man Non-binary      Woman
#    2         34          4         14

# chart of gender breakdown
ggplot(data, aes(x = fct_infreq(gender), fill = fct_infreq(gender))) +
  geom_bar() +
  labs(
    title = "Gender of respondents",
    subtitle = "Other/No response (9999): 2",
    x = NULL,
    y = "# respondents"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# --- EXPERIENCE ---------------------

xp <- table(data$experience.level)
# 1    2    3    4 9999
# 6   12   23   10    3
#
# no response: 3
# 1. somewhat experienced (a few years): 6
# 2. experienced (5-10 years): 12
# 3. very experienced (10-20 years): 23
# 4. expert (20+ years): 10

# experience levels chart

# x-axis labels
xp_labels <- c("<5", "5-10", "10-20", "20+", "no response")

# chart
ggplot(
  data,
  aes(x = factor(experience.level), fill = factor(experience.level))
) +
  geom_bar() +
  scale_x_discrete(labels = xp_labels) +
  labs(title = "Years of experience", x = NULL, y = "# respondents") +
  theme_minimal() +
  theme(legend.position = "none")


# --- DEGREES AND CERTIFICATIONS ------------------

table(data$degrees.and.certifications)
# no response: 10 (18.5%)
# - 9 ("If none, leave blank") + 1  "some college"

# respondents who hold any degree: 39
degree_any <- data |>
  filter(
    str_detect(degrees.and.certifications, "Bachelor") |
      str_detect(degrees.and.certifications, "Master's") |
      str_detect(degrees.and.certifications, "PhD") |
      str_detect(degrees.and.certifications, "Adjacent")
  ) |>
  nrow()

# percentage
degree_any / n_responses * 100
# 72.2%

# -- highest level attained
# abbreviation 'geo' means 'cartography/GIS/geography'

# ------ this section was used to extract strings from the
# ------ google form results but i ended up making a separate
# ------ variable for these later on

# # geo PhD: only 1
# degree_geo_phd <- data |>
#   filter(str_detect(degrees.and.certifications, "PhD")) |>
#   nrow()

# # percentage
# degree_geo_phd / n_responses * 100
# # 1.9% (one person)

# # geo Master's: 20
# degree_geo_masters <- data |>
#   filter(
#     str_detect(degrees.and.certifications, "Master's") &
#       !str_detect(degrees.and.certifications, "PhD")
#   ) |>
#   nrow()

# # percentage
# degree_geo_masters / n_responses * 100
# # 37%

# # geo Bachelor's: 8
# degree_geo_bachelors <- data |>
#   filter(
#     str_detect(degrees.and.certifications, "Bachelor") &
#       !str_detect(degrees.and.certifications, "Master's") &
#       !str_detect(degrees.and.certifications, "PhD")
#   ) |>
#   nrow()

# # percentage
# degree_geo_bachelors / n_responses * 100
# # 14.8%

# ------ end section -------

# lots of people checked off multiple degrees but i only need to know the highest one
table(data$highest.geo.degree)


# -- taking a look at all the degree combos
data |>
  filter(
    str_detect(degrees.and.certifications, "Bachelor") |
      str_detect(degrees.and.certifications, "Master's") |
      str_detect(degrees.and.certifications, "PhD") |
      str_detect(degrees.and.certifications, "Adjacent")
  ) |>
  count(degrees.and.certifications)

# -- anyone with a map-adjacent degree
# e.g. geology, environmental science, visual arts, graphic design

data |>
  filter(str_detect(degrees.and.certifications, "Adjacent")) |>
  count(degrees.and.certifications)
#                               degrees.and.certifications n
#                                          Adjacent Degree 4
# Bachelor;Certificate;Other certification;Adjacent Degree 1
#  Bachelor;GISP certification;Certificate;Adjacent Degree 1
#            Bachelor;Master's;Certificate;Adjacent Degree 1
#                              Certificate;Adjacent Degree 5
#                       GISP certification;Adjacent Degree 1
#                                 Master's;Adjacent Degree 1

# calculate no. of adjacent degree holders (any combo)
degree_adjacent_combo <- data |>
  filter(str_detect(degrees.and.certifications, "Adjacent")) |>
  nrow()

degree_adjacent_combo / n_responses * 100
# total adjacent degree holders (any combo): 14 (25.9%)

# adjacent degree only, no geo-related degree(s): 6 (11.1%)
6 / n_responses * 100

# adjacent degree plus geo-related degree or certification: 10

# certificate(s) only, no degrees (total): 4
certificate_only <- data |>
  filter(
    (str_detect(degrees.and.certifications, "Certificate") |
      str_detect(degrees.and.certifications, "GISP") |
      str_detect(degrees.and.certifications, "Other certification") |
      str_detect(degrees.and.certifications, "Chartered")) &
      !str_detect(degrees.and.certifications, "Bachelor") &
      !str_detect(degrees.and.certifications, "Master's") &
      !str_detect(degrees.and.certifications, "PhD") &
      !str_detect(degrees.and.certifications, "Adjacent")
  ) |>
  nrow()

certificate_only / n_responses * 100
# 7.4%

# degree(s) only, no certificates: 22
degree_only <- data |>
  filter(
    (str_detect(degrees.and.certifications, "Bachelor") |
      str_detect(degrees.and.certifications, "Master's") |
      str_detect(degrees.and.certifications, "PhD") |
      str_detect(degrees.and.certifications, "Adjacent")) &
      !str_detect(degrees.and.certifications, "Certificate") &
      !str_detect(degrees.and.certifications, "GISP") &
      !str_detect(degrees.and.certifications, "Other certification") &
      !str_detect(degrees.and.certifications, "Chartered")
  ) |>
  nrow()

degree_only / n_responses * 100
# 40.7%

# any degree PLUS any certifications: 17
degree_plus_certificate <- data |>
  filter(
    (str_detect(degrees.and.certifications, "Bachelor") |
      str_detect(degrees.and.certifications, "Master's") |
      str_detect(degrees.and.certifications, "PhD") |
      str_detect(degrees.and.certifications, "Adjacent")) &
      (str_detect(degrees.and.certifications, "Certificate") |
        str_detect(degrees.and.certifications, "GISP") |
        str_detect(degrees.and.certifications, "Other certification") |
        str_detect(degrees.and.certifications, "Chartered"))
  ) |>
  nrow()

degree_plus_certificate / n_responses * 100
# 31.5%

# chart

# summary of respondents by highest credential obtained, mutually exclusive
#
# 1. if someone has both a geo and adjacent degree, they're in the geo bucket
# 2. if someone has both an adjacent degree and certificate(s) but no geo degree,
#    they're in the adjacent bucket
#
# one person said "computer science", i added that as a geo-adjacent degree
# one person said "some college", they were added to "none"

degree_cert_summary <- tibble(
  category = c(
    "Geo PhD",
    "Geo Master's",
    "Geo Bachelor's",
    "Adjacent degree",
    "Geo Certificate only",
    "None/no response"
  ),
  n = c(
    degree_geo_phd,
    degree_geo_masters,
    degree_geo_bachelors,
    11,
    certificate_only,
    10
  )
) |>
  mutate(category = factor(category, levels = category))

# sanity check: should equal n_responses (54)
sum(degree_cert_summary$n)

ggplot(
  degree_cert_summary,
  aes(x = n, y = fct_rev(category), fill = fct_rev(category))
) +
  geom_col() +
  labs(
    title = "Degrees and certifications",
    x = "# respondents",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# -----------------------------------
# --- RATES
# -----------------------------------

# typical hourly rate per year

# rate by gender
# rate by experience
# rate vs education

# perception of fair pay
# previous survey influence
# dependence on freelance
# - typical hourly rate vs level of dependence
# - flat rate vs hourly by level of dependence

# -----------------------------------
# --- PROJECT TYPES
# -----------------------------------

# number of projects per person (median)
# number of projects by dependence on freelance

# static or interactive
# number of projects by type
# - other industries
# - project types by gender
# - project types by depenence on freelance
# typical hourly rate (median/mean/range) per project type
