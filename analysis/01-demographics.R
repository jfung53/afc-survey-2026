# -----------------------------------
# AFC 2026 Annual Survey Analysis — demographics
# -----------------------------------

source("/Users/jocelyn/Documents/Pratt/Projects/afc-survey-2026/analysis/00-setup.R")

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
