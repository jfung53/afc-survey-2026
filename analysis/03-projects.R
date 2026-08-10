# -----------------------------------
# AFC 2026 Annual Survey Analysis — project types
# -----------------------------------

source(
  "/Users/jocelyn/Documents/Pratt/Projects/afc-survey-2026/analysis/00-setup.R"
)

# -----------------------------------
# --- PROJECT TYPES
# -----------------------------------

# number of projects per person
# these were exact numbers, not ranges
# excludes the 4 respondents with no response
projects <- data |>
  mutate(projects = na_if(number.of.projects, 9999))

# sanity check: 4 respondents left this field blank
sum(is.na(projects$projects))

median_projects <- median(projects$projects, na.rm = TRUE)
mean_projects <- mean(projects$projects, na.rm = TRUE)

median_projects
# 6.5
mean_projects
# 11.66
# median is well below mean -- distribution is right-skewed (most
# respondents did 1-15 projects, but a handful did 20-95)

# distribution of number of projects
project_dist_plot <- ggplot(projects, aes(x = projects)) +
  geom_histogram(binwidth = 1, boundary = 0.5) +
  geom_vline(
    xintercept = median_projects,
    color = "steelblue",
    linetype = "dashed"
  ) +
  labs(
    title = "Number of projects per person",
    subtitle = "Dashed line: median",
    x = "# projects",
    y = "# respondents"
  ) +
  theme_minimal()

project_dist_plot

ggsave(
  "images/project_distribution.svg",
  plot = project_dist_plot,
  width = 6,
  height = 4
)

# ------ number of projects by industry

# each industry column is bucketed multiple-choice, not a raw count:
# respondents picked one of None/1/2-3/4-5/6+ per industry
# excludes 9999 (left blank) but keeps "None" as a real response
industry_cols <- c(
  Government = "number.of.projects..government.",
  Academic = "number.of.projects..academic.",
  Private = "number.of.projects..private.",
  Environmental = "number.of.projects..environmental.",
  GLAM = "number.of.projects..glam.",
  `Non-profit` = "number.of.projects..non.profit.",
  Publishers = "number.of.projects..publishers.",
  `Design agency` = "number.of.projects..design.agency.",
  Engineering = "number.of.projects..engineering.",
  Resources = "number.of.projects..resources.",
  `Private individuals` = "number.of.projects..private.individuals.",
  Other = "number.of.projects..other."
)

industry_projects <- data |>
  select(id, all_of(industry_cols)) |>
  pivot_longer(-id, names_to = "industry", values_to = "bucket") |>
  filter(bucket != "9999") |>
  mutate(bucket = factor(bucket, levels = c("None", "1", "2–3", "4–5", "6+")))

# sanity check: valid (non-9999) response counts per industry
count(industry_projects, industry)

industry_summary <- industry_projects |>
  count(industry, bucket) |>
  group_by(industry) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

# order industries by % of respondents who did any projects there
# (i.e. bucket != "None"), so charts read as a ranked overview
# NOTE: sum(pct[bucket != "None"]) instead of filter()-then-summarise, so
# industries with zero non-None responses (e.g. Resources) still get a
# row with pct_any = 0 instead of being dropped -- a dropped industry
# would be missing from `levels` below and turn into NA when refactored
industry_order <- industry_summary |>
  summarise(pct_any = sum(pct[bucket != "None"]), .by = industry) |>
  arrange(pct_any) |>
  pull(industry)

industry_summary <- industry_summary |>
  mutate(industry = factor(industry, levels = industry_order))

# heatmap: which industries each respondent is active in, and how much.
# both size and color encode the project-count bucket (redundant on
# purpose); "None" is dropped so it renders as a blank cell rather than
# a filled square

# order respondents by their exact total project count (from `projects`,
# computed above), busiest at top; the 4 respondents with no
# total-projects response sort to the bottom
respondent_order <- projects |>
  arrange(desc(projects)) |>
  pull(id)

# respondents with zero real industry engagement (all None/9999) drop
# out of the chart entirely once "None" rows are filtered out -- discrete
# scales hide unused levels
industry_heatmap_data <- industry_projects |>
  filter(bucket != "None") |>
  mutate(
    industry = factor(industry, levels = industry_order),
    id = factor(id, levels = respondent_order)
  )

# industry_order is left as-is (not reversed): it's ascending by pct_any,
# and ggplot's default discrete y-axis puts the first level at the bottom
# and the last at the top -- so Publishers (the last/highest-pct_any
# level) already lands at the top without any extra reversal
industry_heatmap_plot <- ggplot(
  industry_heatmap_data,
  aes(x = id, y = industry, color = bucket)
) +
  # all points the same (max) size -- only color encodes the bucket now
  geom_point(shape = 15, size = 3.5) +
  # Resources has no real data (everyone who answered picked "None," so
  # it disappears once "None" rows are filtered out above) -- drop =
  # FALSE keeps it as an empty row instead of hiding it
  scale_y_discrete(drop = FALSE) +
  scale_color_brewer(palette = "Reds") +
  labs(
    title = "Industries worked in, per respondent",
    subtitle = "Columns ordered by total # of projects, most to least; blank = none in that industry",
    x = NULL,
    y = NULL,
    color = "# projects"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

industry_heatmap_plot

# wider than the other charts here -- with 54 respondent columns, this is
# the only way to give each column enough horizontal space that the
# largest points (bucket "6+") don't spill into neighboring columns
ggsave(
  "images/industry_heatmap.svg",
  plot = industry_heatmap_plot,
  width = 14,
  height = 6
)

# number of projects by dependence on freelance
# excludes the 2 respondents with no dependence response and the 4
# missing this year's project count
dependence_projects <- data |>
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
    ),
    projects = na_if(number.of.projects, 9999)
  ) |>
  filter(!is.na(projects))

# sanity check: counts should sum to 48 (54 minus 2 missing dependence
# minus however many of the 4 missing-projects respondents overlap)
count(dependence_projects, dependence)
#    Not at all 19
#      Somewhat 11
# Substantially  4
#    Completely 16

# sanity check / median values by group
dependence_projects_median <- dependence_projects |>
  summarise(median_projects = median(projects), .by = dependence)

dependence_projects_median
#    Completely            14.0
#    Not at all             4.0
#      Somewhat             5.0
# Substantially            11.5

dependence_on_freelance_plot <- ggplot(
  dependence_projects,
  aes(x = dependence, y = projects)
) +
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(
    data = dependence_projects_median,
    aes(yintercept = median_projects),
    color = "steelblue",
    linetype = "dashed"
  ) +
  geom_text(
    data = dependence_projects_median,
    aes(y = median_projects, label = median_projects),
    x = -Inf,
    hjust = -0.2,
    vjust = -0.5,
    color = "steelblue",
    size = 3.5
  ) +
  facet_wrap(~dependence, scales = "free_x", nrow = 1) +
  labs(
    title = "Number of projects by dependence on freelance income",
    subtitle = "Dashed line: median # projects for that response",
    x = NULL,
    y = "# projects"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

dependence_on_freelance_plot

ggsave(
  "images/dependence_on_freelance.svg",
  plot = dependence_on_freelance_plot,
  width = 6,
  height = 4
)

# spearman's rho test
cor.test(
  as.numeric(dependence_projects$dependence),
  dependence_projects$projects,
  method = "spearman"
)
# strong, statistically significant correlation: freelancers who depend
# more on freelance income take on more projects
#
# S = 8120.4, p-value = 2.559e-06
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho
# 0.6100636

# ------ static or interactive

# 1 = always/almost always static, 5 = always/almost always interactive
# excludes missing responses
project_type <- data |>
  filter(static.or.interactive != 9999) |>
  mutate(
    project_type = case_when(
      static.or.interactive == 1 ~ "Always/almost always static",
      static.or.interactive == 2 ~ "Usually static",
      static.or.interactive == 3 ~ "About equally likely",
      static.or.interactive == 4 ~ "Usually interactive",
      static.or.interactive == 5 ~ "Always/almost always interactive"
    ),
    project_type = factor(
      project_type,
      levels = c(
        "Always/almost always static",
        "Usually static",
        "About equally likely",
        "Usually interactive",
        "Always/almost always interactive"
      )
    )
  )

# sanity check: should sum to n_responses minus no-response count
count(project_type, project_type)
#      Always/almost always static 25
#                   Usually static 15
#             About equally likely  7
#              Usually interactive  3
# Always/almost always interactive  2

project_type_summary <- project_type |>
  count(project_type) |>
  mutate(pct = n / sum(n) * 100)

static_vs_int_plot <- ggplot(
  project_type_summary,
  aes(x = pct, y = "", fill = project_type)
) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(
    aes(label = paste0(n, "\n", round(pct), "%")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    color = "white",
    size = 3
  ) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title = "Static vs. interactive maps",
    x = "% of respondents",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())

ggsave(
  "images/static_vs_interactive.svg",
  plot = static_vs_int_plot,
  width = 8,
  height = 4
)

# ------ number of projects by type

# note: "Resources" has no real data -- every respondent who answered
# picked "None" -- so it shows as a 100% None bar, which is itself
# informative (nobody in this sample does resource-sector work)
project_types_plot <- ggplot(
  industry_summary,
  aes(x = pct, y = industry, fill = bucket)
) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = "Reds") +
  labs(
    title = "Project industries",
    subtitle = "# of projects per industry, past 12 months",
    x = "% of respondents",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal()

project_types_plot

ggsave(
  "images/project_types.svg",
  plot = project_types_plot,
  width = 8,
  height = 10
)

# ----- project types by gender
# static vs. interactive by gender
# --- note: because of the small numbers, the p-values are not reliable
# (see wilcoxon rank-sum and fisher's tests below)
gender_colors <- setNames(
  scales::hue_pal()(n_distinct(data$gender)),
  levels(fct_infreq(data$gender))
)

project_type_by_gender <- project_type |>
  filter(!gender %in% c("9999", "Non-binary")) |>
  count(gender, project_type) |>
  group_by(gender) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

project_type_by_gender
#   gender project_type                         n   pct
#   <chr>  <fct>                            <int> <dbl>
# 1 Man    Always/almost always static         14 41.2
# 2 Man    Usually static                      10 29.4
# 3 Man    About equally likely                 5 14.7
# 4 Man    Usually interactive                  3  8.82
# 5 Man    Always/almost always interactive     2  5.88
# 6 Woman  Always/almost always static          9 64.3
# 7 Woman  Usually static                       3 21.4
# 8 Woman  About equally likely                 2 14.3

project_type_gender_plot <- ggplot(
  project_type_by_gender,
  aes(x = project_type, y = pct, fill = fct_infreq(gender, w = n))
) +
  # "Usually interactive" and "Always/almost always interactive" only
  # have Man respondents (no Woman rows at all) -- plain position =
  # "dodge" would let that lone bar stretch to fill the space both bars
  # would've shared, instead of staying half-width to match the other
  # categories. preserve = "single" keeps every bar's width fixed at
  # what it'd be with both groups present, missing or not
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = gender_colors) +
  labs(
    title = "Static vs. interactive maps by gender",
    subtitle = "% within each gender (Man, Woman only)",
    x = NULL,
    y = "% of respondents",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

project_type_gender_plot

ggsave(
  "images/project_type_gender.svg",
  plot = project_type_gender_plot,
  width = 6,
  height = 6
)

# q: is the gender difference above statistically significant?
# a: no (see tests below)

project_type_mw <- project_type |> filter(!gender %in% c("9999", "Non-binary"))

# wilcoxon rank-sum test: respects the ordinal 1-5 scale
# rather than treating the categories as unordered
wilcox.test(static.or.interactive ~ gender, data = project_type_mw)
# W = 303, p-value = 0.1161

# fisher's exact test: plain gender x category contingency table
fisher.test(table(
  project_type_mw$gender,
  project_type_mw$static.or.interactive
))
# p-value = 0.6805

# ----- project types by dependence on freelance
# q: does dependence on freelance income predict industry participation?

# number of distinct industries that each respondent participated in
industry_long <- data |>
  select(id, dependence.on.freelance, all_of(industry_cols)) |>
  pivot_longer(
    all_of(names(industry_cols)),
    names_to = "industry",
    values_to = "bucket"
  )
head(industry_long)

# count per respondent id
breadth <- industry_long |>
  filter(!bucket %in% c("None", "9999")) |>
  count(id, name = "n_industries")
head(breadth)

# left-join back onto all respondent ids and fill 0 -- 2 respondents have
# zero real participation across all 12 industries (all None/9999) and
# would otherwise be silently dropped instead of counted as 0
dependence_breadth <- data |>
  select(id, dependence.on.freelance) |>
  left_join(breadth, by = "id") |>
  mutate(n_industries = replace_na(n_industries, 0)) |>
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

# sanity check: counts should sum to 52 (54 minus 2 missing dependence)
count(dependence_breadth, dependence)
#      dependence  n
# 1    Not at all 20
# 2      Somewhat 11
# 3 Substantially  4
# 4    Completely 17

dependence_breadth_median <- dependence_breadth |>
  summarise(median_n_industries = median(n_industries), .by = dependence)

dependence_breadth_median
#      dependence median_n_industries
# 1    Completely                 5.0
# 2    Not at all                 2.0
# 3      Somewhat                 3.0
# 4 Substantially                 3.5

industries_by_dependence_plot <- ggplot(
  dependence_breadth,
  aes(x = dependence, y = n_industries)
) +
  geom_jitter(width = 0.1, height = 0) +
  geom_hline(
    data = dependence_breadth_median,
    aes(yintercept = median_n_industries),
    color = "steelblue",
    linetype = "dashed"
  ) +
  geom_text(
    data = dependence_breadth_median,
    aes(y = median_n_industries, label = median_n_industries),
    x = -Inf,
    hjust = -0.2,
    vjust = -0.5,
    color = "steelblue",
    size = 3.5
  ) +
  facet_wrap(~dependence, scales = "free_x", nrow = 1) +
  labs(
    title = "# of industries worked in, by dependence on freelance income",
    subtitle = "Dashed line: median # industries for that response",
    x = NULL,
    y = "# distinct industries"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

industries_by_dependence_plot

ggsave(
  "images/industries_by_dependence.svg",
  plot = industries_by_dependence_plot,
  width = 6,
  height = 4
)

# q: do those who depend on more on freelance participate in more industries?

# spearman correlation: dependence as an ordered 1-4 scale (factor levels
# run Not at all -> Completely, i.e. increasing dependence intensity, so
# the sign of rho is directly interpretable) vs. # of industries
cor.test(
  as.numeric(dependence_breadth$dependence),
  dependence_breadth$n_industries,
  method = "spearman"
)
# results are statistically significant:
# freelancers who depend more on freelancing tend to work across a
# larger number of industries
#
# S = 11425, p-value = 0.0001034
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho
# 0.5123064

# ------ rate vs. total number of projects
# excludes respondents missing either value (46 have both)
rate_vs_projects <- projects |>
  mutate(rate = na_if(typical.hourly.rate, 9999)) |>
  filter(!is.na(projects), !is.na(rate))

# sanity check: 46 respondents have both a rate and a project count
nrow(rate_vs_projects)

# q: is there a relationship between the number of projects and typical hourly rate?

rate_vs_num_projects_plot <- ggplot(
  rate_vs_projects,
  aes(x = projects, y = rate)
) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue") +
  scale_x_log10() +
  labs(
    title = "Typical hourly rate vs. total number of projects",
    subtitle = "Log scale on # projects due to right-skewed distribution",
    x = "# projects (log scale)",
    y = "Typical hourly rate ($)"
  ) +
  theme_minimal()

rate_vs_num_projects_plot

ggsave(
  "images/rate_vs_num_projects.svg",
  plot = rate_vs_num_projects_plot,
  width = 6,
  height = 4
)

# spearman (not pearson): # of projects is heavily right-skewed, so a
# rank-based correlation is more appropriate than one that assumes a
# linear relationship
cor.test(
  rate_vs_projects$projects,
  rate_vs_projects$rate,
  method = "spearman"
)
# a: weak correlation:
# S = 11676, p-value = 0.05953
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho
# 0.2799418

# ------ industry co-occurrence

# q: which industries tend to be worked by the same freelancers? build a
# respondent x industry binary matrix (active = did >=1 project in that
# industry, i.e. bucket isn't "None" or blank) and cross-tabulate
industry_active_wide <- data |>
  select(id, all_of(industry_cols)) |>
  pivot_longer(-id, names_to = "industry", values_to = "bucket") |>
  mutate(active = !bucket %in% c("None", "9999")) |>
  pivot_wider(id_cols = id, names_from = industry, values_from = active)

industry_mat <- as.matrix(industry_active_wide[, names(industry_cols)]) * 1

# symmetric co-occurrence count matrix: cell (A, B) = # respondents active
# in both A and B; diagonal = total # respondents active in that industry
industry_cooc <- t(industry_mat) %*% industry_mat

# sanity check: diagonal should match the "any" counts underlying
# industry_order above
diag(industry_cooc)

# order industries by hierarchical clustering (binary/Jaccard-style
# distance on the active vectors) so industries that tend to co-occur
# land next to each other on both axes, instead of an arbitrary or
# alphabetical order
industry_clust <- hclust(dist(t(industry_mat), method = "binary"))
industry_clust_order <- colnames(industry_mat)[industry_clust$order]

industry_cooc_long <- as.data.frame(industry_cooc) |>
  mutate(industry_a = rownames(industry_cooc)) |>
  pivot_longer(-industry_a, names_to = "industry_b", values_to = "n") |>
  mutate(
    industry_a = factor(industry_a, levels = industry_clust_order),
    industry_b = factor(industry_b, levels = industry_clust_order)
  )

industry_coocc_plot <- ggplot(
  industry_cooc_long,
  aes(x = industry_b, y = industry_a, fill = n)
) +
  geom_tile() +
  geom_text(aes(label = n), color = "white", size = 3) +
  scale_fill_gradient(low = "grey90", high = "steelblue") +
  labs(
    title = "Industry co-occurrence",
    subtitle = "# of respondents active in both industries (diagonal = total active in that industry)",
    x = NULL,
    y = NULL,
    fill = "# resp."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

industry_coocc_plot

ggsave(
  "images/industry_coocc.svg",
  plot = industry_coocc_plot,
  width = 8,
  height = 6
)

# ------ industry breadth vs. rate

# q: do generalists (more distinct industries) earn a different rate than
# specialists (fewer industries)? reuses `breadth` (id, n_industries)
# computed above for the dependence analysis
# excludes respondents missing a rate (breadth itself has no missing
# values -- non-participants were already filled to 0 there)
breadth_rate <- data |>
  select(id, typical.hourly.rate) |>
  left_join(breadth, by = "id") |>
  mutate(
    n_industries = replace_na(n_industries, 0),
    rate = na_if(typical.hourly.rate, 9999)
  ) |>
  filter(!is.na(rate))

# sanity check: 48 respondents have a rate (54 minus 6 missing typical.hourly.rate)
nrow(breadth_rate)

rate_vs_breadth_plot <- ggplot(breadth_rate, aes(x = n_industries, y = rate)) +
  geom_jitter(width = 0.1, height = 0) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue") +
  labs(
    title = "Typical hourly rate vs. industry breadth",
    subtitle = "# of distinct industries worked in, past 12 months",
    x = "# distinct industries",
    y = "Typical hourly rate ($)"
  ) +
  theme_minimal()

rate_vs_breadth_plot

ggsave(
  "images/rate_vs_breadth.svg",
  plot = rate_vs_breadth_plot,
  width = 6,
  height = 4
)

# spearman (not pearson): breadth is a small-range count, not
# necessarily linear with rate
cor.test(
  breadth_rate$n_industries,
  breadth_rate$rate,
  method = "spearman"
)
# weak correlation:
# S = 13264, p-value = 0.05383
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho
# 0.2800933
