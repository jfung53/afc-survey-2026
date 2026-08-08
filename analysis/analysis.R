# -----------------------------------
# AFC 2026 Annual Survey Analysis
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

# -- GIS/geography/cartography degrees
# -- lots of people checked off more than one but i only need to know the highest one
table(data$highest.geo.degree)
# Bachelors   Masters       PhD
#         8        20         1

29 / n_responses * 100
# 53.7%

# respondents who hold any degree: 39
# includes non-geo degrees
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

# -- extracting highest level attained for charting

degree_by_level <- data |>
  count(highest.geo.degree) |>
  mutate(pct = n / n_responses * 100)

degree_by_level

degree_geo_phd <- degree_by_level |>
  filter(highest.geo.degree == "PhD") |>
  pull(n)

degree_geo_masters <- degree_by_level |>
  filter(highest.geo.degree == "Masters") |>
  pull(n)

degree_geo_bachelors <- degree_by_level |>
  filter(highest.geo.degree == "Bachelors") |>
  pull(n)


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

# ------ adjacent degree only, no geo-related degree(s): 11 (20.4%)
# manually counted
degree_adjacent_no_geo <- 11
degree_adjacent_no_geo / n_responses * 100

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
    degree_adjacent_no_geo,
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

# -- chord diagram: overlap between degrees and certifications
# -- thanks, claude!
# off-diagonal cells count pairwise overlap between different credentials;
# diagonal cells count respondents holding that credential exclusively,
# but are hidden from rendering (link.visible) so exclusive holders show
# up as empty sector space rather than a self-loop

cred <- data |>
  mutate(
    PhD = highest.geo.degree %in% "PhD",
    `Master's` = highest.geo.degree %in% "Masters",
    `Bachelor's` = highest.geo.degree %in% "Bachelors",
    Adjacent = !is.na(adjacent.degree),
    Certificate = !is.na(certificate),
    GISP = !is.na(gisp),
    `Other cert.` = !is.na(other.certification)
  ) |>
  select(
    PhD,
    `Master's`,
    `Bachelor's`,
    Adjacent,
    Certificate,
    GISP,
    `Other cert.`
  )

cats <- names(cred)
n_cat <- length(cats)
mat <- matrix(0, n_cat, n_cat, dimnames = list(cats, cats))
n_creds <- rowSums(cred)

for (i in seq_len(n_cat)) {
  for (j in seq_len(n_cat)) {
    if (i == j) {
      mat[i, j] <- sum(cred[[i]] & n_creds == 1)
    } else {
      mat[i, j] <- sum(cred[[i]] & cred[[j]])
    }
  }
}

# zero out the redundant lower triangle so chordDiagram() plots the
# matrix as directed without doubling every edge weight, while still
# preserving the diagonal values (needed to reserve sector space for
# exclusive holders; symmetric = TRUE drops self-loop-only sectors,
# e.g. PhD)
mat_upper <- mat
mat_upper[lower.tri(mat_upper)] <- 0

# add an 8th "None" sector for the 10 respondents with no degrees or
# certificates at all; give it a diagonal value only (no overlaps by
# definition) so it reserves sector space but stays empty
mat8 <- matrix(
  0,
  n_cat + 1,
  n_cat + 1,
  dimnames = list(c(cats, "None"), c(cats, "None"))
)
mat8[cats, cats] <- mat_upper
mat8["None", "None"] <- 10

# hide the diagonal links from rendering, leaving empty arc space for
# exclusive holders (and the None sector) instead of drawing a self-loop
vis8 <- mat8 != 0
diag(vis8) <- FALSE

sector_order <- c(
  "PhD",
  "Master's",
  "Bachelor's",
  "Adjacent",
  "Certificate",
  "GISP",
  "Other cert.",
  "None"
)

circos.clear()
circos.par(start.degree = 180)
chordDiagram(
  mat8,
  self.link = 1,
  link.visible = vis8,
  order = sector_order,
  annotationTrack = c("name", "grid"),
  grid.col = c(
    "PhD" = "#1b9e77",
    "Master's" = "#1b9e77",
    "Bachelor's" = "#1b9e77",
    "Adjacent" = "#d95f02",
    "Certificate" = "#7570b3",
    "GISP" = "#7570b3",
    "Other cert." = "#7570b3",
    "None" = "grey60"
  )
)

# note to self: maybe omit the ticks, too messy
# custom axis: one tick per two respondents, rather than the default
# spacing, to keep tick labels legible
# circos.track(
#   track.index = 2,
#   panel.fun = function(x, y) {
#     xlim <- get.cell.meta.data("xlim")
#     sector <- get.cell.meta.data("sector.index")
#     circos.axis(
#       h = "top",
#       major.at = seq(0, xlim[2], by = 2),
#       minor.ticks = 0,
#       labels.cex = 0.4,
#       sector.index = sector
#     )
#   },
#   bg.border = NA
# )

title("Many respondents hold multiple degrees and certifications")
circos.clear()

# -----------------------------------
# --- RATES
# -----------------------------------

# typical hourly rate per year
# rate by gender

# calculate typical hourly rate median for 2026

# exclude "9999" (no response) as missing before taking medians
rate_2026 <- data |>
  mutate(rate = na_if(typical.hourly.rate, 9999))

# sanity check: 6 respondents left this field blank
sum(is.na(rate_2026$rate))

median_all_2026 <- median(rate_2026$rate, na.rm = TRUE)
median_men_2026 <- median(
  rate_2026$rate[rate_2026$gender == "Man"],
  na.rm = TRUE
)
median_women_2026 <- median(
  rate_2026$rate[rate_2026$gender == "Woman"],
  na.rm = TRUE
)

median_hourly_rate <- tribble(
  ~year , ~group  , ~median_hourly_rate ,
   2018 , "all"   ,                  60 ,
   2018 , "men"   ,                  68 ,
   2018 , "women" ,                  50 ,
   2020 , "all"   ,                  73 ,
   2020 , "men"   ,                  75 ,
   2020 , "women" ,                  60 ,
   2022 , "all"   ,                  65 ,
   2022 , "men"   ,                  60 ,
   2022 , "women" ,                  75 ,
   2024 , "all"   ,                  65 ,
   2024 , "men"   ,                  65 ,
   2024 , "women" ,                  60 ,
   2025 , "all"   ,                  60 ,
   2025 , "men"   ,                  65 ,
   2025 , "women" ,                  50
) |>
  add_row(year = 2026, group = "all", median_hourly_rate = median_all_2026) |>
  add_row(year = 2026, group = "men", median_hourly_rate = median_men_2026) |>
  add_row(
    year = 2026,
    group = "women",
    median_hourly_rate = median_women_2026
  )

# chart: median hourly rate by year per gender
survey_years <- respondents_by_year$year

ggplot(
  median_hourly_rate,
  aes(x = year, y = median_hourly_rate, color = group)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = survey_years,
    labels = \(x) ifelse(x %in% median_hourly_rate$year, x, ""),
    limits = range(survey_years),
    expand = expansion(mult = 0.03)
  ) +
  labs(
    title = "Median hourly rate by year",
    x = NULL,
    y = "Median hourly rate ($)",
    color = NULL
  ) +
  theme_minimal() +
  theme(axis.ticks.x = element_line(color = "grey50"))

# typical hourly rate compared to previous year

# scatter plot: typical hourly rate per respondent (thanks, claude!)
# x-axis order (by this year's rate) stays fixed regardless of change type
# - arrows show direction of change vs. previous year
# - no change: keep the (filled) circle
# - previous.typical.hourly.rate == 9999 (no response): outlined circle
gender_colors <- setNames(
  scales::hue_pal()(n_distinct(data$gender)),
  levels(fct_infreq(data$gender))
)

# NOTE: when several layers each supply their own filtered subset of the
# same factor column, ggplot2's discrete position scale compacts levels
# per layer rather than as one shared union, which silently breaks a
# consistent left-to-right order across layers. Using a plain numeric
# rank (shared identically by all layers) avoids that.
rate_arrows <- rate_2026 |>
  filter(!is.na(rate)) |>
  mutate(
    prev_rate = na_if(previous.typical.hourly.rate, 9999),
    change_type = case_when(
      previous.typical.hourly.rate == 9999 ~ "no_prev",
      rate == prev_rate ~ "same",
      TRUE ~ "changed"
    ),
    rank = rank(rate, ties.method = "first")
  )

# sanity check: 18 changed, 22 stayed the same, 8 have no previous-year rate
count(rate_arrows, change_type)

rate_change_plot <- ggplot() +
  geom_segment(
    data = filter(rate_arrows, change_type == "changed"),
    aes(
      x = rank,
      y = prev_rate,
      xend = rank,
      yend = rate,
      color = fct_infreq(gender)
    ),
    arrow = arrow(length = unit(0.1, "inches"), type = "closed")
  ) +
  geom_point(
    data = filter(rate_arrows, change_type == "same"),
    aes(x = rank, y = rate, color = fct_infreq(gender))
  ) +
  geom_point(
    data = filter(rate_arrows, change_type == "no_prev"),
    aes(x = rank, y = rate, color = fct_infreq(gender)),
    shape = 21,
    fill = NA
  ) +
  scale_color_manual(values = gender_colors) +
  labs(
    title = "This year's hourly rate vs last year's",
    x = "Respondent (ordered by rate, low to high)",
    y = "Typical hourly rate ($)",
    color = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# view scatter plot
rate_change_plot

# save as svg
ggsave(
  "hourly-rate-change.svg",
  plot = rate_change_plot,
  device = "svg",
  width = 10,
  height = 5
)

# rate by education
# mutually exclusive education categories, similar to the earlier
# degree/certification breakdown: highest geo degree takes priority,
# then adjacent degree, then certificate(s) only, then no response
# excludes the 6 respondents missing this year's rate
rate_by_education <- rate_2026 |>
  filter(!is.na(rate)) |>
  mutate(
    education = case_when(
      highest.geo.degree == "PhD" ~ "PhD",
      highest.geo.degree == "Masters" ~ "Masters",
      highest.geo.degree == "Bachelors" ~ "Bachelors",
      !is.na(adjacent.degree) ~ "Adjacent",
      !is.na(certificate) | !is.na(gisp) | !is.na(other.certification) ~
        "Certificates only",
      TRUE ~ "No response"
    ),
    education = factor(
      education,
      levels = c(
        "PhD",
        "Masters",
        "Bachelors",
        "Adjacent",
        "Certificates only",
        "No response"
      )
    )
  )

# sanity check: counts should sum to 48 (54 minus 6 missing rate)
count(rate_by_education, education)

rate_by_education_plot <- ggplot(
  rate_by_education,
  aes(x = education, y = rate)
) +
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(
    data = rate_by_education |>
      summarise(median_rate = median(rate), .by = education),
    aes(yintercept = median_rate),
    color = "steelblue",
    linetype = "dashed"
  ) +
  facet_wrap(~education, scales = "free_x", nrow = 1) +
  labs(
    title = "Typical hourly rate by education",
    subtitle = "Dashed line: median rate for that education category",
    x = NULL,
    y = "Typical hourly rate ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

rate_by_education_plot

# rate by experience
# excludes the 6 respondents missing this year's rate
rate_by_experience <- rate_2026 |>
  filter(!is.na(rate)) |>
  mutate(
    experience = case_when(
      experience.level == 1 ~ "<5 yrs",
      experience.level == 2 ~ "5-10 yrs",
      experience.level == 3 ~ "10-20 yrs",
      experience.level == 4 ~ "20+ yrs",
      TRUE ~ "No response"
    ),
    experience = factor(
      experience,
      levels = c("<5 yrs", "5-10 yrs", "10-20 yrs", "20+ yrs", "No response")
    )
  )

# sanity check: counts should sum to 48 (54 minus 6 missing rate)
count(rate_by_experience, experience)

rate_by_experience_plot <- ggplot(
  rate_by_experience,
  aes(x = experience, y = rate)
) +
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(
    data = rate_by_experience |>
      summarise(median_rate = median(rate), .by = experience),
    aes(yintercept = median_rate),
    color = "steelblue",
    linetype = "dashed"
  ) +
  facet_wrap(~experience, scales = "free_x", nrow = 1) +
  labs(
    title = "Typical hourly rate by experience level",
    subtitle = "Dashed line: median rate for that experience level",
    x = NULL,
    y = "Typical hourly rate ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

rate_by_experience_plot


# perception of fair pay
table(data$perception.of.fair.pay)

# 4 Never: 1
# 3 Rarely: 9
# 2 Some of the time: 20
# 1 Most or all of the time: 22
# 9999 (no response): 2

fair_pay <- data |>
  mutate(
    fair_pay = case_when(
      perception.of.fair.pay == 1 ~ "Most or all of the time",
      perception.of.fair.pay == 2 ~ "Some of the time",
      perception.of.fair.pay == 3 ~ "Rarely",
      perception.of.fair.pay == 4 ~ "Never",
      TRUE ~ "No response"
    ),
    fair_pay = factor(
      fair_pay,
      levels = c(
        "Most or all of the time",
        "Some of the time",
        "Rarely",
        "Never",
        "No response"
      )
    )
  )

ggplot(fair_pay, aes(x = fair_pay)) +
  geom_bar() +
  labs(
    title = "Perception of fair pay",
    x = NULL,
    y = "# respondents"
  ) +
  theme_minimal()

# typical hourly rate vs. perception of fair pay
# excludes respondents missing this year's rate (6) or a fair pay
# response (2, folded into "No response" upstream but dropped here
# since none of them also reported a rate)
fair_pay_rate <- fair_pay |>
  mutate(rate = na_if(typical.hourly.rate, 9999)) |>
  filter(!is.na(rate))

# sanity check: 21/19/7/1 across most-of-the-time/some/rarely/never
count(fair_pay_rate, fair_pay)

ggplot(fair_pay_rate, aes(x = fair_pay, y = rate)) +
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(
    data = fair_pay_rate |>
      summarise(median_rate = median(rate), .by = fair_pay),
    aes(yintercept = median_rate),
    color = "steelblue",
    linetype = "dashed"
  ) +
  facet_wrap(~fair_pay, scales = "free_x", nrow = 1) +
  labs(
    title = "Typical hourly rate by perception of fair pay",
    subtitle = "Dashed line: median rate for that response",
    x = NULL,
    y = "Typical hourly rate ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# perception of fair pay by gender
# uses % within each gender rather than raw counts, since gender group
# sizes are very unequal (Man 34, Woman 14); excludes the 2 respondents
# with no gender response and the 4 non-binary respondents (too few to
# compare reliably)
fair_pay_by_gender <- fair_pay |>
  filter(!gender %in% c("9999", "Non-binary")) |>
  count(gender, fair_pay) |>
  group_by(gender) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

fair_pay_by_gender

ggplot(
  fair_pay_by_gender,
  aes(x = fair_pay, y = pct, fill = fct_infreq(gender, w = n))
) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = gender_colors) +
  labs(
    title = "Perception of fair pay by gender",
    subtitle = "% within each gender (Man, Woman only)",
    x = NULL,
    y = "% of respondents",
    fill = "Gender"
  ) +
  theme_minimal()

# previous survey influence on business practices or rates
table(data$influence.of.previous.surveys)
# yes: 23
# no: 13
# had not seen previous: 17
# no response: 1

influence <- data |>
  mutate(
    influence = case_when(
      influence.of.previous.surveys == "Yes" ~ "Yes",
      influence.of.previous.surveys == "No" ~ "No",
      influence.of.previous.surveys ==
        "I haven't seen any previous survey results" ~
        "Haven't seen previous results",
      TRUE ~ "No response"
    ),
    influence = factor(
      influence,
      levels = c("Yes", "No", "Haven't seen previous results", "No response")
    )
  )

ggplot(influence, aes(x = influence)) +
  geom_bar() +
  labs(
    title = "Influence of previous survey results",
    x = NULL,
    y = "# respondents"
  ) +
  theme_minimal()

# dependence on freelance
table(data$dependence.on.freelance, useNA = "always")

# 1 I depended on it completely; freelance mapping was necessary for my survival
# -- respondents: 17
# 2 I depended on it substantially; it would have been possible, but challenging, to survive without my freelance income
# -- respondents: 4
# 3 I depended on it somewhat; it would not have been difficult to survive without freelance income, but it helped make it easier
# -- respondents: 11
# 4 I did not depend on it at all; mapping provided surplus income
# -- respondents: 20
# 9999 no response: 2

# excludes the 2 respondents with no response
dependence <- data |>
  filter(dependence.on.freelance != 9999) |>
  mutate(
    dependence = case_when(
      dependence.on.freelance == 1 ~ "Completely",
      dependence.on.freelance == 2 ~ "Substantially",
      dependence.on.freelance == 3 ~ "Somewhat",
      dependence.on.freelance == 4 ~ "Not at all"
    ),
    dependence = factor(
      dependence,
      levels = c("Not at all", "Somewhat", "Substantially", "Completely")
    )
  )

ggplot(dependence, aes(x = dependence)) +
  geom_bar() +
  labs(
    title = "Dependence on freelance income",
    x = NULL,
    y = "# respondents"
  ) +
  theme_minimal()

# - typical hourly rate vs level of dependence
# excludes the 2 respondents with no dependence response (via
# `dependence`, already filtered) and the 6 missing this year's rate
dependence_rate <- dependence |>
  mutate(rate = na_if(typical.hourly.rate, 9999)) |>
  filter(!is.na(rate))

# sanity check: 17/10/4/17 across not at all/somewhat/substantially/completely
count(dependence_rate, dependence)

ggplot(dependence_rate, aes(x = dependence, y = rate)) +
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(
    data = dependence_rate |>
      summarise(median_rate = median(rate), .by = dependence),
    aes(yintercept = median_rate),
    color = "steelblue",
    linetype = "dashed"
  ) +
  facet_wrap(~dependence, scales = "free_x", nrow = 1) +
  labs(
    title = "Typical hourly rate by dependence on freelance income",
    subtitle = "Dashed line: median rate for that response",
    x = NULL,
    y = "Typical hourly rate ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# billing method: flat rate vs hourly

# - flat rate vs hourly by level of dependence
# hourly.or.flat.rate: "Do you charge an hourly rate or a flat rate for
# your freelance cartography work?"
# 1 = always/almost always hourly,
# 5 = always/almost always flat rate)
billing_by_dependence <- dependence |>
  filter(hourly.or.flat.rate != 9999) |>
  mutate(
    billing = case_when(
      hourly.or.flat.rate == 1 ~ "Always/almost always hourly",
      hourly.or.flat.rate == 2 ~ "Usually hourly",
      hourly.or.flat.rate == 3 ~ "About equally likely",
      hourly.or.flat.rate == 4 ~ "Usually flat rate",
      hourly.or.flat.rate == 5 ~ "Always/almost always flat rate"
    ),
    billing = factor(
      billing,
      levels = c(
        "Always/almost always hourly",
        "Usually hourly",
        "About equally likely",
        "Usually flat rate",
        "Always/almost always flat rate"
      )
    )
  ) |>
  count(dependence, billing) |>
  group_by(dependence) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

billing_by_dependence

# this chart doesn't show any clear patterns, it's kind of all over the place,
# probably because of the small number of respondents.
# not sure if we should use it?
ggplot(billing_by_dependence, aes(x = dependence, y = billing, fill = pct)) +
  geom_tile() +
  geom_text(aes(label = round(pct)), color = "white", size = 3.5) +
  scale_fill_gradient(low = "grey90", high = "steelblue") +
  labs(
    title = "Billing method by dependence on freelance income",
    subtitle = "% within each dependence level",
    x = NULL,
    y = NULL,
    fill = "%"
  ) +
  theme_minimal()

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
