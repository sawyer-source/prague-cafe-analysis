# ===================================================================
# 02_descriptive.R -- DESCRIBE THE DATA AND DECIDE: PARAMETRIC OR NOT?
# (topic: Cafe Size)
#
# what this script does, for every variable our questions use (and for
# every group a question compares):
#   1. counts and percentages of the categorical variables
#   2. count, missing values, mean, SD, median, IQR and range
#   3. histogram, boxplot and Q-Q plot
#   4. normality check (skewness)
#   5. central limit theorem check (group size + bootstrapped means)
#   6. one table that says: parametric or non-parametric
#
# it reads analysis/dataset.csv (made by 01_cleaning.R) and writes
#   tables:  report/tables/D_counts.tex, D_stats.tex, D_table.tex
#   figures: report/figures/D_histograms.pdf, D_boxplots.pdf, D_qq.pdf,
#            D_bootstrap.pdf
#
# open prague-cafe-analysis.Rproj first, so the paths below work
# ===================================================================

library(dplyr)     # group_by(), summarise(), filter()
library(ggplot2)   # the plots
library(xtable)    # tables -> LaTeX

df <- read.csv(file.path("analysis", "dataset.csv"))

# folders for the exported tables and figures (created if they don't exist yet)
dir_tab <- file.path("report", "tables")
dir_fig <- file.path("report", "figures")
dir.create(dir_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. CATEGORICAL VARIABLES: COUNTS AND PERCENTAGES
# ============================================================
# a categorical variable has no mean or SD -- counts and percentages are
# all we describe. it also needs no normality check

# a small function: counts and % of every value (NA shown as "missing")
describe_categories <- function(x, variable) {
  counts <- table(x, useNA = "ifany")
  values <- names(counts)
  values[is.na(values)] <- "missing"
  data.frame(variable = variable,
             value    = values,
             n        = as.vector(counts),
             percent  = round(100 * as.vector(counts) / sum(counts), 1))
}

# the 0/1 dummies get readable labels: 0 = "no", 1 = "yes"
counts_table <- rbind(
  describe_categories(factor(df$has_outdoor, levels = c(0, 1), labels = c("no", "yes")),
                      "Outdoor seating"),
  describe_categories(factor(df$small_cafe, levels = c(0, 1), labels = c("no", "yes")),
                      "Small cafe (15 or fewer seats inside)"),
  describe_categories(factor(df$top_rated, levels = c(0, 1), labels = c("no", "yes")),
                      "Top-rated (Google rating 4.7 or higher)"),
  describe_categories(factor(df$above_median_price, levels = c(0, 1), labels = c("no", "yes")),
                      "Above-median Price Level"),
  # districts in number order (1, 2, 3 ...), not alphabetical (1, 10, 11, 2 ...)
  describe_categories(factor(df$district, levels = paste("Prague", c(1, 2, 3, 5, 7, 8, 9, 10, 11))),
                      "District")
)
counts_table


# ============================================================
# 2. NUMERIC VARIABLES: ONE ROW PER THING WE WILL TEST
# ============================================================
# the guidelines want every numeric variable described overall AND within
# every group a question compares. so we stack them into one long data
# frame: "row" says which variable/group a value belongs to, "used_in"
# says which question needs it. every table and plot below has one entry
# per row

# A4 compares only these six districts
df_a4 <- filter(df, district %in% c("Prague 1", "Prague 2", "Prague 3",
                                    "Prague 5", "Prague 7", "Prague 8"))

series <- rbind(
  # A1, A6, A7, A10: seats inside, all cafes
  data.frame(row = "Seats inside, all cafes", used_in = "A1, A6, A7, A10",
             value = df$seats_inside),
  # A2: seats inside, compared between cafes with and without outdoor seating
  data.frame(row = "Seats inside, with outdoor seating", used_in = "A2",
             value = filter(df, has_outdoor == 1)$seats_inside),
  data.frame(row = "Seats inside, no outdoor seating", used_in = "A2",
             value = filter(df, has_outdoor == 0)$seats_inside),
  # A3: a PAIRED comparison, so we describe the DIFFERENCE inside - outside
  data.frame(row = "Seats inside - outside (difference)", used_in = "A3",
             value = df$seats_inside - df$seats_outside),
  # A4: seats outside, in each of the six districts
  data.frame(row = "Seats outside, Prague 1", used_in = "A4",
             value = filter(df_a4, district == "Prague 1")$seats_outside),
  data.frame(row = "Seats outside, Prague 2", used_in = "A4",
             value = filter(df_a4, district == "Prague 2")$seats_outside),
  data.frame(row = "Seats outside, Prague 3", used_in = "A4",
             value = filter(df_a4, district == "Prague 3")$seats_outside),
  data.frame(row = "Seats outside, Prague 5", used_in = "A4",
             value = filter(df_a4, district == "Prague 5")$seats_outside),
  data.frame(row = "Seats outside, Prague 7", used_in = "A4",
             value = filter(df_a4, district == "Prague 7")$seats_outside),
  data.frame(row = "Seats outside, Prague 8", used_in = "A4",
             value = filter(df_a4, district == "Prague 8")$seats_outside),
  # A6-A10: the Price Level
  data.frame(row = "Price Level (CZK)", used_in = "A6-A10",
             value = df$price_level),
  # A8: distance from the Orloj
  data.frame(row = "Distance from the Orloj (km)", used_in = "A8",
             value = df$distance_km)
)

# keep the rows in the order written above (not alphabetical) in tables and plots
series$row <- factor(series$row, levels = unique(series$row))


# ============================================================
# 3. COUNT AND SUMMARISE
# ============================================================
stats_table <- series %>%
  group_by(row) %>%
  summarise(n       = sum(!is.na(value)),   # observations we can use
            missing = sum(is.na(value)),    # missing values (NA)
            mean    = mean(value, na.rm = TRUE),
            sd      = sd(value, na.rm = TRUE),
            median  = median(value, na.rm = TRUE),
            iqr     = IQR(value, na.rm = TRUE),   # middle 50%: 3rd quartile - 1st quartile
            min     = min(value, na.rm = TRUE),
            max     = max(value, na.rm = TRUE))
stats_table

# from here on we only need the values that are not missing
series_clean <- series[!is.na(series$value), ]


# ============================================================
# 4. PLOT EVERY ROW: HISTOGRAM, BOXPLOT, Q-Q PLOT
# ============================================================
# facet_wrap() draws one small panel per row, side by side.
# scales = "free": every panel gets its own axes (seats and km differ)

# histogram: the shape of the distribution. normal = one symmetric bell
plot_hist <- ggplot(series_clean, aes(x = value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ row, scales = "free") +
  labs(x = "value (seats, CZK or km)", y = "number of cafes") +
  theme_minimal()
plot_hist
ggsave(file.path(dir_fig, "D_histograms.pdf"), plot_hist, width = 11, height = 8)

# boxplot: median (thick line), middle 50% (box) and outliers (dots)
plot_box <- ggplot(series_clean, aes(x = "", y = value)) +
  geom_boxplot() +
  facet_wrap(~ row, scales = "free") +
  labs(x = NULL, y = "value (seats, CZK or km)") +
  theme_minimal()
plot_box
ggsave(file.path(dir_fig, "D_boxplots.pdf"), plot_box, width = 11, height = 8)

# Q-Q plot: if the data are normal, the points follow the straight line.
# points curving away from the line at the ends = skew or heavy tails
plot_qq <- ggplot(series_clean, aes(sample = value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ row, scales = "free") +
  labs(x = "theoretical quantiles (normal distribution)", y = "sample quantiles") +
  theme_minimal()
plot_qq
ggsave(file.path(dir_fig, "D_qq.pdf"), plot_qq, width = 11, height = 8)


# ============================================================
# 5. NORMALITY CHECK: SKEWNESS
# ============================================================
# skewness measures asymmetry: 0 = symmetric like the normal curve,
# positive = long tail to the right (a few very large values).
# the formula from 05_assessing-normality.R:
skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3

# RULE (a common rule of thumb): |skewness| < 0.5 counts as roughly
# symmetric, i.e. normal enough. we read it together with the plots above


# ============================================================
# 6. CENTRAL LIMIT THEOREM CHECK: GROUP SIZE + BOOTSTRAPPED MEANS
# ============================================================
# even if the VALUES are not normal, the MEAN of a large enough sample is
# roughly normal (the CLT) -- and the mean is what parametric tests use.
# two checks, as the guidelines ask:
#   (1) at least 30 observations in the group (rule of thumb)
#   (2) the distribution of bootstrapped means looks normal
#
# bootstrapping (07_bootstrapping.R): treat our sample as the population,
# draw a new sample of the SAME size from it WITH replacement, take its
# mean, and repeat 5,000 times. the 5,000 means show how the mean would
# vary from sample to sample. seed 123 makes the "random" draws the same
# on every computer
n_boot <- 5000

bootstrap_means <- function(x) {
  set.seed(123)
  replicate(n_boot, mean(sample(x, length(x), replace = TRUE)))
}

# loop over the rows: bootstrap each one and collect the results
d_table   <- NULL   # one line per row: the decision table
boot_data <- NULL   # all bootstrapped means, for the plot
for (r in levels(series_clean$row)) {
  x     <- series_clean$value[series_clean$row == r]
  means <- bootstrap_means(x)

  boot_data <- rbind(boot_data, data.frame(row = r, boot_mean = means))
  d_table   <- rbind(d_table, data.frame(
    variable      = r,
    used_in       = series_clean$used_in[series_clean$row == r][1],
    n             = length(x),
    skewness      = skewness(x),        # of the values themselves
    boot_skewness = skewness(means)     # of the 5,000 bootstrapped means
  ))
}

boot_data$row <- factor(boot_data$row, levels = levels(series_clean$row))

# the bootstrapped means: a symmetric bell = the CLT works for that row
plot_boot <- ggplot(boot_data, aes(x = boot_mean)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ row, scales = "free") +
  labs(x = "bootstrapped mean (5,000 resamples, seed 123)", y = "count") +
  theme_minimal()
plot_boot
ggsave(file.path(dir_fig, "D_bootstrap.pdf"), plot_boot, width = 11, height = 8)


# ============================================================
# 7. THE DECISION: PARAMETRIC OR NON-PARAMETRIC
# ============================================================
# parametric if the values are normal OR the CLT applies; otherwise
# non-parametric. a question that compares groups can use the parametric
# test only if EVERY group it compares says "parametric"
d_table$normal     <- ifelse(abs(d_table$skewness) < 0.5, "yes", "no")
d_table$clt        <- ifelse(d_table$n >= 30 & abs(d_table$boot_skewness) < 0.5, "yes", "no")
d_table$conclusion <- ifelse(d_table$normal == "yes" | d_table$clt == "yes",
                             "parametric", "non-parametric")
d_table


# ============================================================
# 8. EXPORT THE TABLES TO LATEX
# ============================================================
# floating = FALSE: only the table itself; the caption is added in the
# report's .tex file around it. booktabs = TRUE: clean horizontal rules.
# comment = FALSE: no timestamp line, so every run gives the same file
names(counts_table) <- c("Variable", "Value", "n", "%")
print(xtable(counts_table, digits = c(0, 0, 0, 0, 1)),
      file = file.path(dir_tab, "D_counts.tex"),
      floating = FALSE, booktabs = TRUE, include.rownames = FALSE, comment = FALSE)

# the guidelines: round estimates to two decimals and prices to one decimal.
# so we turn the numbers into text: 2 decimals, but 1 for the Price Level row
# (it is a price). sprintf("%.1f", x) writes x with 1 decimal, "%.2f" with 2.
price_row <- grepl("Price Level", stats_table$row)
for (col in c("mean", "sd", "median", "iqr", "min", "max")) {
  stats_table[[col]] <- ifelse(price_row,
                               sprintf("%.1f", stats_table[[col]]),
                               sprintf("%.2f", stats_table[[col]]))
}

names(stats_table) <- c("Variable (group)", "n", "Missing", "Mean", "SD",
                        "Median", "IQR", "Min", "Max")
print(xtable(stats_table, align = c("l", "P{3.6cm}", rep("r", 8))),
      file = file.path(dir_tab, "D_stats.tex"),
      floating = FALSE, booktabs = TRUE, include.rownames = FALSE, comment = FALSE)

names(d_table) <- c("Variable (group)", "Used in", "n", "Skewness",
                    "Boot. skewness", "Normal", "CLT", "Conclusion")
# P{3.6cm}: a 3.6 cm wide first column whose text wraps, so the table fits the
# page (the P column type is defined at the top of report.tex)
print(xtable(d_table, digits = 2,
             align = c("l", "P{3.6cm}", "P{1.6cm}", "r", "r", "r", "l", "l", "l")),
      file = file.path(dir_tab, "D_table.tex"),
      floating = FALSE, booktabs = TRUE, include.rownames = FALSE, comment = FALSE)
