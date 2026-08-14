# -----------------------------------
# AFC 2026 Annual Survey Analysis — rates, fairness, dependence, billing
# -----------------------------------

source(
  "/Users/jocelyn/Documents/Pratt/Projects/afc-survey-2026/analysis/00-setup.R"
)

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

# markdown table: median hourly rate by year, all/men/women
median_hourly_rate_table <- median_hourly_rate |>
  pivot_wider(names_from = group, values_from = median_hourly_rate) |>
  rename(Year = year, All = all, Men = men, Women = women)

cat(knitr::kable(median_hourly_rate_table, format = "pipe"), sep = "\n")

# chart: median hourly rate by year per gender
survey_years <- respondents_by_year$year

rate_by_survey_year_plot <- ggplot(
  median_hourly_rate,
  aes(x = year, y = median_hourly_rate, color = group)
) +
  geom_line() +
  geom_text(
    aes(label = median_hourly_rate),
    vjust = -0.8,
    show.legend = FALSE
  ) +
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

rate_by_survey_year_plot

ggsave(
  "images/rate_by_survey_year.svg",
  plot = rate_by_survey_year_plot,
  width = 6,
  height = 4
)

# typical hourly rate compared to previous year

# scatter plot: typical hourly rate per respondent (thanks, claude!)
# x-axis order (by this year's rate) stays fixed regardless of change type
# - arrows show direction of change vs. previous year
# - no change: keep the (filled) circle
# - previous.typical.hourly.rate == 9999 (no response): outlined circle
# - note: too much info
gender_colors <- setNames(
  scales::hue_pal()(n_distinct(data$gender)),
  levels(fct_infreq(data$gender))
)
# this is me asking claude how to change the order of points
#
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

# rate change summary: # and % who increased, decreased, or had no change
# excludes the 8 respondents with no previous-year rate (change_type == "no_prev")
rate_change_summary <- rate_arrows |>
  filter(change_type != "no_prev") |>
  mutate(
    direction = case_when(
      rate > prev_rate ~ "Increased",
      rate < prev_rate ~ "Decreased",
      TRUE ~ "No change"
    ),
    direction = factor(
      direction,
      levels = c("Increased", "No change", "Decreased")
    )
  ) |>
  count(direction) |>
  mutate(pct = n / sum(n) * 100)

# sanity check: 16 increased, 22 no change, 2 decreased (sums to 40)
rate_change_summary

rate_change_summary_plot <- ggplot(
  rate_change_summary,
  aes(x = direction, y = n)
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = paste0(n, " (", round(pct), "%)")),
    vjust = -0.5
  ) +
  labs(
    title = "Change in typical hourly rate vs. last year",
    subtitle = "Excludes respondents with no previous-year rate",
    x = NULL,
    y = "# respondents"
  ) +
  theme_minimal()

rate_change_summary_plot

# save as svg
ggsave(
  "images/rate_change_summary.svg",
  plot = rate_change_summary_plot,
  width = 6,
  height = 4
)

# calculate average increase amount, (ignores no change/decrease)
avg_rate_increase <- rate_arrows |>
  filter(rate > prev_rate) |>
  summarise(avg_increase = mean(rate - prev_rate)) |>
  pull(avg_increase)

# $13.38
avg_rate_increase


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

# save as svg
ggsave(
  "images/rate_by_education.svg",
  plot = rate_by_education_plot,
  width = 6,
  height = 4
)


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

# save as svg
ggsave(
  "images/rate_by_experience.svg",
  plot = rate_by_experience_plot,
  width = 6,
  height = 4
)

# spearman's rho test
cor.test(
  as.numeric(rate_by_experience$experience),
  rate_by_experience$rate,
  method = "spearman"
)
# essentially no correlation, not statistically significant: in this
# sample, rate does not track with experience level
#
# S = 16805, p-value = 0.5527
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho
# 0.08785623

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

fair_pay_plot <- ggplot(fair_pay, aes(x = fair_pay)) +
  geom_bar() +
  labs(
    title = "Perception of fair pay",
    x = NULL,
    y = "# respondents"
  ) +
  theme_minimal()

fair_pay_plot

# save as svg
ggsave(
  "images/fair_pay.svg",
  plot = fair_pay_plot,
  width = 6,
  height = 4
)

# typical hourly rate vs. perception of fair pay
# excludes respondents missing this year's rate (6) or a fair pay
# response (2, folded into "No response" upstream but dropped here
# since none of them also reported a rate)
fair_pay_rate <- fair_pay |>
  mutate(rate = na_if(typical.hourly.rate, 9999)) |>
  filter(!is.na(rate))

# sanity check: 21/19/7/1 across most-of-the-time/some/rarely/never
count(fair_pay_rate, fair_pay)

fair_pay_rate_plot <- ggplot(fair_pay_rate, aes(x = fair_pay, y = rate)) +
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

fair_pay_rate_plot

# save as svg
ggsave(
  "images/fair_pay_rate.svg",
  plot = fair_pay_rate_plot,
  width = 6,
  height = 4
)

# spearman's rho test
cor.test(
  as.numeric(fair_pay_rate$fair_pay),
  fair_pay_rate$rate,
  method = "spearman"
)
# moderate, statistically significant negative correlation: respondents
# who perceive fair pay less often tend to report lower rates
#
# S = 26830, p-value = 0.001116
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho
# -0.4562761

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

fair_pay_by_gender_plot <- ggplot(
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

fair_pay_by_gender_plot

# save as svg
ggsave(
  "images/fair_pay_by_gender.svg",
  plot = fair_pay_by_gender_plot,
  width = 6,
  height = 4
)

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

influence_plot <- ggplot(influence, aes(x = influence)) +
  geom_bar() +
  labs(
    title = "Influence of previous survey results",
    x = NULL,
    y = "# respondents"
  ) +
  theme_minimal()

influence_plot

# save as svg
ggsave(
  "images/influence.svg",
  plot = influence_plot,
  width = 6,
  height = 4
)

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

dependence_plot <- ggplot(dependence, aes(x = dependence)) +
  geom_bar() +
  labs(
    title = "Dependence on freelance income",
    x = NULL,
    y = "# respondents"
  ) +
  theme_minimal()

dependence_plot

# save as svg
ggsave(
  "images/dependence.svg",
  plot = dependence_plot,
  width = 6,
  height = 4
)

# - typical hourly rate vs level of dependence
# excludes the 2 respondents with no dependence response (via
# `dependence`, already filtered) and the 6 missing this year's rate
dependence_rate <- dependence |>
  mutate(rate = na_if(typical.hourly.rate, 9999)) |>
  filter(!is.na(rate))

# sanity check: 17/10/4/17 across not at all/somewhat/substantially/completely
count(dependence_rate, dependence)

dependence_rate_plot <- ggplot(dependence_rate, aes(x = dependence, y = rate)) +
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

dependence_rate_plot

ggsave(
  "images/dependence_rate.png",
  plot = dependence_rate_plot,
  width = 6,
  height = 4
)

# spearman's rho test
cor.test(
  as.numeric(dependence_rate$dependence),
  dependence_rate$rate,
  method = "spearman"
)
# weak but statistically significant positive correlation: freelancers
# who depend more on freelance income tend to report a higher rate
#
# S = 13051, p-value = 0.0443
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho
# 0.2916458

# billing method: flat rate vs hourly

# 1 = always/almost always hourly,
# 5 = always/almost always flat rate)
# excludes missing responses
billing <- data |>
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
  )

# sanity check: should sum to n_responses minus no-response count
count(billing, billing)

billing_summary <- billing |>
  count(billing) |>
  mutate(pct = n / sum(n) * 100)

billing_plot <- ggplot(billing_summary, aes(x = pct, y = "", fill = billing)) +
  geom_col() +
  geom_text(
    aes(label = paste0(n, "\n", round(pct), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title = "Billing method",
    x = "% of respondents",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())

billing_plot

ggsave(
  "images/billing_method.svg",
  plot = billing_plot,
  width = 12,
  height = 4
)

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
billing_by_dependence <- ggplot(
  billing_by_dependence,
  aes(x = dependence, y = billing, fill = pct)
) +
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

billing_by_dependence

ggsave(
  "images/billing_by_dependence.svg",
  plot = billing_by_dependence,
  width = 8,
  height = 6
)

# spearman's rho test
billing_dependence_indiv <- dependence |>
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
  )

cor.test(
  as.numeric(billing_dependence_indiv$dependence),
  as.numeric(billing_dependence_indiv$billing),
  method = "spearman"
)
# weak, not statistically significant -- confirms the chart above doesn't
# show a real pattern, likely just noise from the small/sparse cells
#
# S = 24763, p-value = 0.3996
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho
# -0.1204994
