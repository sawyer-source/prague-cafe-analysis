# ===================================================================
# 03_analysis.R -- ANSWER THE TEN CAFE SIZE QUESTIONS (A1-A10)
#
# one block per question, always in the same order:
#   question and hypotheses (written in step 2, before looking at the data)
#   -> the data for this question (and how many cafes were lost to NA)
#   -> the method chosen in 02_descriptive.R (report/tables/D_table.tex)
#   -> checks of the other assumptions of that method
#   -> the test: estimate, 95% CI, effect size, test statistic, p-value
#   -> a table (report/tables/A#_table.tex) and a figure (report/figures/A#_figure.pdf)
#
# significance level: alpha = .05 for every test
# a note on INDEPENDENCE: every test below assumes the cafes are
# independent. cafes in the same district are not fully independent (they
# share rents, tourists and competition), so the p-values may be a bit too
# small and the CIs a bit too narrow. this is discussed in the report
#
# packages used in class; install once with install.packages() if missing
# ===================================================================

library(dplyr)            # filter(), mutate(), group_by(), summarise()
library(ggplot2)          # figures
library(xtable)           # tables -> LaTeX
library(rstatix)          # levene_test(), dunn_test()
library(lmtest)           # bptest(), coeftest(), coefci()
library(sandwich)         # vcovHC(): robust standard errors
library(emmeans)          # emtrends(): the slope in each group (A9)
library(marginaleffects)  # avg_comparisons(): marginal effects of a logit (A10)

df <- read.csv(file.path("analysis", "dataset.csv"))
nrow(df)   # 189 cafes

dir_tab <- file.path("report", "tables")
dir_fig <- file.path("report", "figures")
dir.create(dir_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)

n_boot <- 5000   # bootstrap resamples (at least 5,000, as the guidelines ask), seed 123


# ============================================================
# 0. SMALL HELPER FUNCTIONS (used in every block)
# ============================================================
# numbers -> text with a fixed number of decimals: fmt(3.14159) -> "3.14"
fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)

# a 95% CI as text: "[23.78, 32.20]"
fmt_ci <- function(lower, upper, digits = 2) {
  paste0("[", fmt(lower, digits), ", ", fmt(upper, digits), "]")
}

# p-values as the guidelines want: three decimals, no leading zero
# (p = .034), and "< .001" for very small values
fmt_p <- function(p) {
  ifelse(p < 0.001, "< .001", sub("^0", "", fmt(p, 3)))
}

# the same, as text with the sign: "p = .034" or "p < .001"
p_text <- function(p) ifelse(p < 0.001, "p < .001", paste0("p = ", fmt_p(p)))

# coefficients like r without the leading zero: 0.15 -> ".15", -0.21 -> "-.21"
fmt_r <- function(x) sub("^(-?)0\\.", "\\1.", fmt(x, 2))

# write a data frame as a LaTeX table (the caption is added in report.tex).
# align (optional) sets the columns: "l" = left, "r" = right, "P{3cm}" = a
# 3 cm wide column whose text wraps onto a new line (for long text)
export_table <- function(table, file, align = NULL) {
  print(xtable(table, align = align), file = file.path(dir_tab, file),
        floating = FALSE, booktabs = TRUE, include.rownames = FALSE,
        comment = FALSE)   # no timestamp line, so every run gives the same file
}

# bootstrapped 95% percentile CI of the median (07_bootstrapping.R idea)
boot_median_ci <- function(x) {
  set.seed(123)
  medians <- replicate(n_boot, median(sample(x, length(x), replace = TRUE)))
  quantile(medians, c(0.025, 0.975))
}

# regressions: the guidelines want robust standard errors ONLY if the
# variance of the residuals is not constant. the Breusch-Pagan test checks
# exactly that (H0: constant variance). small p -> use robust (HC3) SEs
choose_vcov <- function(model) {
  bp <- bptest(model)
  if (bp$p.value < 0.05) vcovHC(model, type = "HC3") else vcov(model)
}

# a regression table: estimate, SE, 95% CI, t and p for every coefficient
regression_rows <- function(model, vc, model_name, term_names) {
  ct <- coeftest(model, vcov = vc)       # estimates, SEs, t, p (with the chosen SEs)
  ci <- coefci(model, vcov = vc)         # 95% CIs (with the chosen SEs)
  data.frame(Model       = model_name,
             Term        = term_names,
             Estimate    = fmt(ct[, 1]),
             SE          = fmt(ct[, 2]),
             `95% CI`    = fmt_ci(ci[, 1], ci[, 2]),
             t           = fmt(ct[, 3]),
             p           = fmt_p(ct[, 4]),
             check.names = FALSE)
}

# the summary table of all ten analyses is filled in block by block
summary_rows <- NULL
add_summary <- function(analysis, method, estimate, effect, test, p) {
  summary_rows <<- rbind(summary_rows, data.frame(
    Analysis = analysis, Method = method, `Estimate [95% CI]` = estimate,
    `Effect size` = effect, Test = test, p = p, check.names = FALSE))
}


# ============================================================
# A1. ONE SAMPLE: is the average number of seats inside different from 30?
# ============================================================
# H0: mean seats inside = 30      H1: mean seats inside != 30 (two-tailed)
# method (D_table): parametric through the CLT -> one-sample t-test
a1 <- filter(df, !is.na(seats_inside))
a1_lost <- nrow(df) - nrow(a1)   # cafes lost to missing values
c(n = nrow(a1), lost = a1_lost)

a1_test <- t.test(a1$seats_inside, mu = 30)
a1_test

# effect size: Cohen's d = (mean - 30) / SD -- how far from 30 in SDs
a1_d <- (mean(a1$seats_inside) - 30) / sd(a1$seats_inside)

# seats are skewed, so the median and IQR are reported next to the mean
a1_table <- data.frame(
  n = nrow(a1), Lost = a1_lost,
  Mean = fmt(a1_test$estimate),
  `95% CI` = fmt_ci(a1_test$conf.int[1], a1_test$conf.int[2]),
  Median = fmt(median(a1$seats_inside)), IQR = fmt(IQR(a1$seats_inside)),
  `Cohen's d` = fmt(a1_d),
  t = paste0("t(", a1_test$parameter, ") = ", fmt(a1_test$statistic)),
  p = fmt_p(a1_test$p.value),
  check.names = FALSE)
a1_table
export_table(a1_table, "A1_table.tex")

# figure: every cafe as a dot, the mean with its 95% CI, and 30 as a dashed line.
# position_jitter() spreads the dots sideways so they don't hide each other:
# height = 0 keeps every dot at its true number of seats, seed = 123 makes
# the spread the same on every run
a1_plot <- ggplot(a1, aes(x = "", y = seats_inside)) +
  geom_point(position = position_jitter(width = 0.15, height = 0, seed = 123), alpha = 0.3) +
  geom_errorbar(aes(ymin = a1_test$conf.int[1], ymax = a1_test$conf.int[2]),
                width = 0.1, linewidth = 1, color = "#2a78d6") +
  geom_point(aes(y = a1_test$estimate), size = 3, color = "#2a78d6") +
  geom_hline(yintercept = 30, linetype = "dashed", color = "#e34948") +
  labs(x = NULL, y = "Seats inside (number of seats)") +
  theme_minimal()
a1_plot
ggsave(file.path(dir_fig, "A1_figure.pdf"), a1_plot, width = 6, height = 4)

add_summary("A1 Seats inside vs 30", "One-sample t-test",
            paste(fmt(a1_test$estimate), "seats", fmt_ci(a1_test$conf.int[1], a1_test$conf.int[2])),
            paste("d =", fmt(a1_d)),
            paste0("t(", a1_test$parameter, ") = ", fmt(a1_test$statistic)),
            fmt_p(a1_test$p.value))


# ============================================================
# A2. TWO GROUPS: do cafes with outdoor seating have MORE seats inside?
# ============================================================
# H0: mean(with) <= mean(without)    H1: mean(with) > mean(without) (one-tailed)
# direction of the subtraction: WITH outdoor seating MINUS WITHOUT
# method (D_table): both groups parametric through the CLT -> Welch t-test
a2 <- filter(df, !is.na(seats_inside), !is.na(has_outdoor))
a2_lost <- nrow(df) - nrow(a2)

# R treats the FIRST factor level as group 1, and alternative = "greater"
# means "group 1 > group 2" -- so "with outdoor seating" must come first
a2$outdoor <- factor(a2$has_outdoor, levels = c(1, 0),
                     labels = c("With outdoor seating", "No outdoor seating"))
table(a2$outdoor)

# assumption check: equal variances? (Levene's test, H0: equal variances)
# Welch's t-test does not need equal variances, so it is safe either way
a2_levene <- levene_test(a2, seats_inside ~ outdoor)
a2_levene

# the one-tailed p-value (fixed in advance) ...
a2_test <- t.test(seats_inside ~ outdoor, data = a2, alternative = "greater")
# ... and the two-sided 95% CI, which the guidelines want shown anyway
a2_ci <- t.test(seats_inside ~ outdoor, data = a2)$conf.int
a2_test

# effect size: Cohen's d with the average SD (matches Welch, as in 02_two-sample.R)
with_seats    <- a2$seats_inside[a2$outdoor == "With outdoor seating"]
without_seats <- a2$seats_inside[a2$outdoor == "No outdoor seating"]
a2_diff <- mean(with_seats) - mean(without_seats)
a2_d    <- a2_diff / sqrt((sd(with_seats)^2 + sd(without_seats)^2) / 2)

a2_table <- data.frame(
  n = paste(length(with_seats), "+", length(without_seats)), Lost = a2_lost,
  `Mean with` = fmt(mean(with_seats)), `Mean without` = fmt(mean(without_seats)),
  Difference = fmt(a2_diff),                         # with - without
  `95% CI` = fmt_ci(a2_ci[1], a2_ci[2]),               # two-sided
  d = fmt(a2_d),
  t = paste0("t(", fmt(a2_test$parameter), ") = ", fmt(a2_test$statistic)),
  p = fmt_p(a2_test$p.value),                          # one-tailed
  check.names = FALSE)
a2_table
export_table(a2_table, "A2_table.tex")

# the mean of each group with its 95% CI, for the figure
a2_means <- a2 %>%
  group_by(outdoor) %>%
  summarise(m = mean(seats_inside),
            lower = t.test(seats_inside)$conf.int[1],
            upper = t.test(seats_inside)$conf.int[2])

a2_plot <- ggplot(a2, aes(x = outdoor, y = seats_inside)) +
  geom_point(position = position_jitter(width = 0.15, height = 0, seed = 123), alpha = 0.3) +
  geom_errorbar(data = a2_means, aes(y = m, ymin = lower, ymax = upper),
                width = 0.1, linewidth = 1, color = "#2a78d6") +
  geom_point(data = a2_means, aes(y = m), size = 3, color = "#2a78d6") +
  labs(x = NULL, y = "Seats inside (number of seats)") +
  theme_minimal()
a2_plot
ggsave(file.path(dir_fig, "A2_figure.pdf"), a2_plot, width = 6, height = 4)

add_summary("A2 Seats inside: outdoor vs none", "Welch t-test (one-tailed)",
            paste(fmt(a2_diff), "seats", fmt_ci(a2_ci[1], a2_ci[2])),
            paste("d =", fmt(a2_d)),
            paste0("t(", fmt(a2_test$parameter), ") = ", fmt(a2_test$statistic)),
            fmt_p(a2_test$p.value))


# ============================================================
# A3. PAIRED: how many more seats does a cafe have inside than outside?
# ============================================================
# H0: mean(inside - outside) <= 0    H1: mean(inside - outside) > 0 (one-tailed)
# PAIRED: both numbers come from the same cafe, so we work with the
# difference inside - outside of every cafe
# method (D_table): the differences are parametric through the CLT -> paired t-test
a3 <- filter(df, !is.na(seats_inside), !is.na(seats_outside))
a3_lost <- nrow(df) - nrow(a3)
a3$difference <- a3$seats_inside - a3$seats_outside

# paired = TRUE with two vectors: the difference is always first minus second
a3_test <- t.test(a3$seats_inside, a3$seats_outside, paired = TRUE, alternative = "greater")
a3_ci   <- t.test(a3$seats_inside, a3$seats_outside, paired = TRUE)$conf.int   # two-sided CI
a3_test

# effect size for paired data: d_z = mean difference / SD of the differences
a3_dz <- mean(a3$difference) / sd(a3$difference)

a3_table <- data.frame(
  n = nrow(a3), Lost = a3_lost,
  `Mean inside` = fmt(mean(a3$seats_inside)), `Mean outside` = fmt(mean(a3$seats_outside)),
  Difference = fmt(mean(a3$difference)),             # inside - outside
  `95% CI` = fmt_ci(a3_ci[1], a3_ci[2]),               # two-sided
  `d_z` = fmt(a3_dz),
  t = paste0("t(", a3_test$parameter, ") = ", fmt(a3_test$statistic)),
  p = fmt_p(a3_test$p.value),                          # one-tailed
  check.names = FALSE)
a3_table
export_table(a3_table, "A3_table.tex")

# figure: the guidelines ask to plot the DIFFERENCES for a paired comparison
a3_plot <- ggplot(a3, aes(x = "", y = difference)) +
  geom_point(position = position_jitter(width = 0.15, height = 0, seed = 123), alpha = 0.3) +
  geom_errorbar(aes(ymin = a3_ci[1], ymax = a3_ci[2]),
                width = 0.1, linewidth = 1, color = "#2a78d6") +
  geom_point(aes(y = mean(difference)), size = 3, color = "#2a78d6") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#e34948") +
  labs(x = NULL, y = "Seats inside - seats outside (per cafe)") +
  theme_minimal()
a3_plot
ggsave(file.path(dir_fig, "A3_figure.pdf"), a3_plot, width = 6, height = 4)

add_summary("A3 Seats inside - outside", "Paired t-test (one-tailed)",
            paste(fmt(mean(a3$difference)), "seats", fmt_ci(a3_ci[1], a3_ci[2])),
            paste("d_z =", fmt(a3_dz)),
            paste0("t(", a3_test$parameter, ") = ", fmt(a3_test$statistic)),
            fmt_p(a3_test$p.value))


# ============================================================
# A4. MORE GROUPS: do outdoor seats differ between Prague 1, 2, 3, 5, 7, 8?
# ============================================================
# H0: the six districts have the same outdoor seats
# H1: at least one district differs (no direction)
# method (D_table): Prague 2, 3 and 5 have fewer than 30 cafes and are not
# normal -> NON-parametric for the whole comparison -> Kruskal-Wallis,
# then Dunn's test with Holm's correction if it is significant
a4_districts <- paste("Prague", c(1, 2, 3, 5, 7, 8))
a4_all <- filter(df, district %in% a4_districts)          # the cafes in these districts
a4 <- filter(a4_all, !is.na(seats_outside))
a4_lost <- nrow(a4_all) - nrow(a4)                         # lost to missing values
c(n = nrow(a4), lost = a4_lost)
a4$district <- factor(a4$district, levels = a4_districts)
table(a4$district)

# Kruskal-Wallis ranks all cafes together and compares the average rank of
# each district. no effect size is required (guidelines)
a4_test <- kruskal.test(seats_outside ~ district, data = a4)
a4_test

# the median of each district with a bootstrapped 95% CI (seed 123)
a4_groups <- NULL
for (d in a4_districts) {
  x  <- a4$seats_outside[a4$district == d]
  ci <- boot_median_ci(x)
  a4_groups <- rbind(a4_groups, data.frame(
    district = d, n = length(x), median = median(x), lower = ci[1], upper = ci[2]))
}
a4_groups$district <- factor(a4_groups$district, levels = a4_districts)
a4_groups

a4_table <- data.frame(
  District = a4_groups$district, n = a4_groups$n,
  Median = fmt(a4_groups$median),
  `95% CI (bootstrap)` = fmt_ci(a4_groups$lower, a4_groups$upper),
  check.names = FALSE)
# the test itself goes in the last line of the table
a4_table <- rbind(a4_table, data.frame(
  District = "Kruskal-Wallis", n = nrow(a4), Median = "",
  `95% CI (bootstrap)` = paste0("H(", a4_test$parameter, ") = ", fmt(a4_test$statistic),
                                ", ", p_text(a4_test$p.value)),
  check.names = FALSE))
a4_table
export_table(a4_table, "A4_table.tex")

# POST HOC (the test is significant): which districts differ?
# Dunn's test compares every pair of districts (15 pairs) and Holm's
# correction adjusts the p-values for testing 15 pairs at once
a4_dunn <- dunn_test(a4, seats_outside ~ district, p.adjust.method = "holm")
a4_dunn

# for every pair: the difference in medians (2nd minus 1st) with a
# bootstrapped 95% CI -- resampling within each district separately
a4_posthoc <- NULL
for (i in 1:nrow(a4_dunn)) {
  x1 <- a4$seats_outside[a4$district == a4_dunn$group1[i]]
  x2 <- a4$seats_outside[a4$district == a4_dunn$group2[i]]
  set.seed(123)
  diffs <- replicate(n_boot, median(sample(x2, length(x2), replace = TRUE)) -
                              median(sample(x1, length(x1), replace = TRUE)))
  a4_posthoc <- rbind(a4_posthoc, data.frame(
    Pair = paste(a4_dunn$group2[i], "-", a4_dunn$group1[i]),
    `Difference in medians` = fmt(median(x2) - median(x1)),
    `95% CI (bootstrap)` = fmt_ci(quantile(diffs, 0.025), quantile(diffs, 0.975)),
    z = fmt(a4_dunn$statistic[i]),
    `p (Holm)` = fmt_p(a4_dunn$p.adj[i]),
    check.names = FALSE))
}
a4_posthoc
export_table(a4_posthoc, "A4_posthoc.tex")

# figure: every cafe as a dot, and each district's median with its bootstrapped CI
a4_plot <- ggplot(a4, aes(x = district, y = seats_outside)) +
  geom_point(position = position_jitter(width = 0.15, height = 0, seed = 123), alpha = 0.3) +
  geom_errorbar(data = a4_groups, aes(y = median, ymin = lower, ymax = upper),
                width = 0.15, linewidth = 1, color = "#2a78d6") +
  geom_point(data = a4_groups, aes(y = median), size = 3, color = "#2a78d6") +
  labs(x = NULL, y = "Seats outside (number of seats)") +
  theme_minimal()
a4_plot
ggsave(file.path(dir_fig, "A4_figure.pdf"), a4_plot, width = 7, height = 4)

# for the summary table: the pair with the smallest Holm-adjusted p-value
a4_best <- which.min(a4_dunn$p.adj)
add_summary("A4 Seats outside by district", "Kruskal-Wallis + Dunn (Holm)",
            paste0(a4_posthoc$Pair[a4_best], ": ", a4_posthoc$`Difference in medians`[a4_best],
                   " seats ", a4_posthoc$`95% CI (bootstrap)`[a4_best]),
            "not required",
            paste0("H(", a4_test$parameter, ") = ", fmt(a4_test$statistic)),
            fmt_p(a4_test$p.value))


# ============================================================
# A5. TWO CATEGORICAL: is a small cafe MORE LIKELY to be top-rated?
# ============================================================
# small cafe = 15 or fewer seats inside; top-rated = Google rating 4.7 or higher
# H0: share top-rated among small cafes <= among other cafes
# H1: share top-rated among small cafes > among other cafes (one-tailed)
# direction: SMALL cafes MINUS OTHER cafes, in percentage points
a5 <- filter(df, !is.na(small_cafe), !is.na(top_rated))
a5_lost <- nrow(df) - nrow(a5)
c(n = nrow(a5), lost = a5_lost)
a5$size   <- factor(a5$small_cafe, levels = c(1, 0), labels = c("Small cafe", "Other cafe"))
a5$rating <- factor(a5$top_rated,  levels = c(1, 0), labels = c("Top-rated", "Not top-rated"))

a5_counts <- table(a5$size, a5$rating)   # the contingency table
a5_counts
round(100 * prop.table(a5_counts, margin = 1), 1)   # row percentages

# assumption check: the chi-square test needs every EXPECTED count >= 5,
# otherwise Fisher's exact test is used
a5_chi <- chisq.test(a5_counts, correct = FALSE)
a5_chi$expected
min(a5_chi$expected)   # well above 5 -> chi-square is fine

# the difference in the share of top-rated cafes, with its 95% CI.
# prop.test() gives the same chi-square as above and can be one-tailed
top   <- a5_counts[, "Top-rated"]                 # top-rated cafes in each group
sizes <- rowSums(a5_counts)                        # cafes in each group
a5_test <- prop.test(top, sizes, alternative = "greater", correct = FALSE)   # one-tailed p
a5_ci   <- prop.test(top, sizes, correct = FALSE)$conf.int                   # two-sided CI
a5_test
a5_diff <- 100 * (a5_test$estimate[1] - a5_test$estimate[2])   # in percentage points

# effect size: Cramer's V (0 = no association, 1 = perfect), from the
# chi-square above: V = sqrt(chi2 / (N x (smaller table dimension - 1)))
a5_v <- sqrt(unname(a5_chi$statistic) / (nrow(a5) * (min(dim(a5_counts)) - 1)))

# post hoc: standardised residuals; |value| > 2 marks the cells that drive it
round(a5_chi$stdres, 2)

a5_table <- data.frame(
  Group = rownames(a5_counts),
  `Top-rated` = as.vector(a5_counts[, "Top-rated"]),
  `Not top-rated` = as.vector(a5_counts[, "Not top-rated"]),
  `% top-rated` = fmt(100 * a5_counts[, "Top-rated"] / sizes, 1),
  `Std. residual (top-rated)` = fmt(a5_chi$stdres[, "Top-rated"]),
  check.names = FALSE)
a5_table
export_table(a5_table, "A5_table.tex")

# the test in a second, small table
a5_result <- data.frame(
  `Difference (pp)` = fmt(a5_diff, 1),                                   # small - other
  `95% CI` = fmt_ci(100 * a5_ci[1], 100 * a5_ci[2], 1),                  # two-sided
  `Cramer's V` = fmt_r(a5_v),
  `Smallest expected count` = fmt(min(a5_chi$expected)),
  `Chi-square` = paste0("chi2(1, N = ", nrow(a5), ") = ", fmt(a5_chi$statistic)),
  p = fmt_p(a5_test$p.value),                                            # one-tailed
  check.names = FALSE)
a5_result
export_table(a5_result, "A5_test.tex")

# figure: the share of top-rated cafes in each group, with its 95% CI
a5_shares <- data.frame(
  size  = factor(rownames(a5_counts), levels = rownames(a5_counts)),
  share = 100 * top / sizes,
  lower = c(100 * prop.test(top[1], sizes[1])$conf.int[1], 100 * prop.test(top[2], sizes[2])$conf.int[1]),
  upper = c(100 * prop.test(top[1], sizes[1])$conf.int[2], 100 * prop.test(top[2], sizes[2])$conf.int[2]),
  label = paste0(top, " of ", sizes))

a5_plot <- ggplot(a5_shares, aes(x = size, y = share)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, linewidth = 1, color = "#2a78d6") +
  geom_point(size = 3, color = "#2a78d6") +
  geom_text(aes(label = label), nudge_x = 0.25) +
  labs(x = NULL, y = "Top-rated cafes (%, rating 4.7 or higher)") +
  ylim(0, 100) +
  theme_minimal()
a5_plot
ggsave(file.path(dir_fig, "A5_figure.pdf"), a5_plot, width = 6, height = 4)

add_summary("A5 Small cafes top-rated", "Chi-square (one-tailed)",
            paste(fmt(a5_diff, 1), "pp", fmt_ci(100 * a5_ci[1], 100 * a5_ci[2], 1)),
            paste("V =", fmt_r(a5_v)),
            paste0("chi2(1, N = ", nrow(a5), ") = ", fmt(a5_chi$statistic)),
            fmt_p(a5_test$p.value))


# ============================================================
# A6. CORRELATION: is the Price Level correlated with the seats inside?
# ============================================================
# H0: no correlation (rho = 0)      H1: rho != 0 (two-tailed)
# method (D_table): both variables parametric through the CLT -> Pearson's r
a6 <- filter(df, !is.na(price_level), !is.na(seats_inside))
a6_lost <- nrow(df) - nrow(a6)

a6_test <- cor.test(a6$seats_inside, a6$price_level)   # Pearson by default
a6_test
a6_r <- unname(a6_test$estimate)

a6_table <- data.frame(
  n = nrow(a6), Lost = a6_lost,
  r = fmt_r(a6_r),
  `95% CI` = paste0("[", fmt_r(a6_test$conf.int[1]), ", ", fmt_r(a6_test$conf.int[2]), "]"),
  `r squared (shared variance)` = fmt_r(a6_r^2),
  t = paste0("t(", a6_test$parameter, ") = ", fmt(a6_test$statistic)),
  p = fmt_p(a6_test$p.value),
  check.names = FALSE)
a6_table
export_table(a6_table, "A6_table.tex")

# figure: the scatterplot first (guidelines), with r and its CI written on it
a6_plot <- ggplot(a6, aes(x = seats_inside, y = price_level)) +
  geom_point(alpha = 0.4) +
  annotate("text", x = max(a6$seats_inside), y = max(a6$price_level), hjust = 1,
           label = paste0("r = ", fmt_r(a6_r), ", 95% CI [", fmt_r(a6_test$conf.int[1]),
                          ", ", fmt_r(a6_test$conf.int[2]), "]")) +
  labs(x = "Seats inside (number of seats)", y = "Price Level (CZK)") +
  theme_minimal()
a6_plot
ggsave(file.path(dir_fig, "A6_figure.pdf"), a6_plot, width = 6, height = 4)

add_summary("A6 Price Level and seats", "Pearson correlation",
            paste0("r = ", fmt_r(a6_r), " [", fmt_r(a6_test$conf.int[1]), ", ", fmt_r(a6_test$conf.int[2]), "]"),
            paste("r2 =", fmt_r(a6_r^2)),
            paste0("t(", a6_test$parameter, ") = ", fmt(a6_test$statistic)),
            fmt_p(a6_test$p.value))


# ============================================================
# A7. SIMPLE REGRESSION: by how many CZK does the Price Level change per 10 seats inside?
# ============================================================
# H0: slope = 0      H1: slope != 0 (two-tailed)
# seats_10 = seats inside divided by 10, so the slope is "CZK per 10 seats"
# (the same trick as I(wage_eur / 10) in 01_simple-regression.R)
a7 <- filter(df, !is.na(price_level), !is.na(seats_inside))
a7_lost <- nrow(df) - nrow(a7)
c(n = nrow(a7), lost = a7_lost)
a7$seats_10 <- a7$seats_inside / 10

a7_model <- lm(price_level ~ seats_10, data = a7)
summary(a7_model)

# CHECKS (02_regression-diagnostics.R): residuals vs fitted, Q-Q plot of
# the residuals, and leverage / Cook's distance -- saved for the appendix
pdf(file.path(dir_fig, "A7_diagnostics.pdf"), width = 10, height = 3.5)
par(mfrow = c(1, 3))
plot(a7_model, which = c(1, 2, 5))
dev.off()

# equal variance? Breusch-Pagan test (H0: constant variance)
bptest(a7_model)
a7_vcov <- choose_vcov(a7_model)   # classic SEs unless the test rejects

# influential cafes: Cook's distance above 4 / n (a rough flag)
a7$cooks <- cooks.distance(a7_model)
sum(a7$cooks > 4 / nrow(a7))
a7 %>% arrange(desc(cooks)) %>% select(cafe, seats_inside, price_level, cooks) %>% head(5)

# does the conclusion change without the flagged cafes? (refit and compare)
a7_without <- lm(price_level ~ seats_10, data = filter(a7, cooks <= 4 / nrow(a7)))
rbind(all_cafes = coef(a7_model), without_flagged = coef(a7_without))

a7_table <- regression_rows(a7_model, a7_vcov, "A7", c("Intercept", "Seats inside (per 10)"))
a7_table <- rbind(a7_table, data.frame(
  Model = "A7", Term = "Observations / R squared", Estimate = nrow(a7), SE = "",
  `95% CI` = paste("R2 =", fmt(summary(a7_model)$r.squared)), t = "", p = "",
  check.names = FALSE))
a7_table
export_table(a7_table, "A7_table.tex")

# figure: the data, the fitted line and its 95% confidence band
a7_plot <- ggplot(a7, aes(x = seats_inside, y = price_level)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#2a78d6") +
  labs(x = "Seats inside (number of seats)", y = "Price Level (CZK)") +
  theme_minimal()
a7_plot
ggsave(file.path(dir_fig, "A7_figure.pdf"), a7_plot, width = 6, height = 4)

a7_ci <- coefci(a7_model, vcov = a7_vcov)["seats_10", ]
a7_ct <- coeftest(a7_model, vcov = a7_vcov)["seats_10", ]
add_summary("A7 Price Level per 10 seats", "Simple regression",
            paste(fmt(a7_ct[1]), "CZK", fmt_ci(a7_ci[1], a7_ci[2])),
            paste("R2 =", fmt(summary(a7_model)$r.squared)),
            paste0("t(", a7_model$df.residual, ") = ", fmt(a7_ct[3])),
            fmt_p(a7_ct[4]))


# ============================================================
# A8. MULTIPLE REGRESSION: does the A7 link hold once distance from the Orloj is included?
# ============================================================
# H0: seats slope = 0 (distance held fixed)   H1: seats slope != 0 (two-tailed)
# same cafes as A7 (no distance is missing), so the two models compare fairly
a8 <- a7
a8_model <- lm(price_level ~ seats_10 + distance_km, data = a8)
summary(a8_model)

# CHECKS: the same diagnostic plots, equal variance, influential cafes
pdf(file.path(dir_fig, "A8_diagnostics.pdf"), width = 10, height = 3.5)
par(mfrow = c(1, 3))
plot(a8_model, which = c(1, 2, 5))
dev.off()
bptest(a8_model)
a8_vcov <- choose_vcov(a8_model)

a8$cooks <- cooks.distance(a8_model)
a8 %>% arrange(desc(cooks)) %>% select(cafe, seats_inside, distance_km, price_level, cooks) %>% head(5)

# are the two predictors too alike? VIF = 1 / (1 - R2 of seats on distance)
# (04_multiple-regression.R). 1 = no overlap, above 5 = a problem
a8_vif <- 1 / (1 - summary(lm(seats_10 ~ distance_km, data = a8))$r.squared)
a8_vif

# does adding distance improve the model? F-test of the nested models
a8_f <- anova(a7_model, a8_model)
a8_f

# how much of the A7 slope remains?
c(A7_slope = unname(coef(a7_model)["seats_10"]), A8_slope = unname(coef(a8_model)["seats_10"]))

a8_table <- rbind(
  regression_rows(a7_model, a7_vcov, "A7 (seats only)", c("Intercept", "Seats inside (per 10)")),
  regression_rows(a8_model, a8_vcov, "A8 (+ distance)", c("Intercept", "Seats inside (per 10)", "Distance (km)")),
  data.frame(Model = "A8 (+ distance)", Term = "Observations / R squared", Estimate = nrow(a8), SE = "",
             `95% CI` = paste("R2 =", fmt(summary(a8_model)$r.squared)), t = "", p = "",
             check.names = FALSE),
  data.frame(Model = "A8 vs A7", Term = "F-test of distance / VIF", Estimate = "", SE = "",
             `95% CI` = paste0("F(", a8_f$Df[2], ", ", a8_f$Res.Df[2], ") = ", fmt(a8_f$F[2])),
             t = paste("VIF =", fmt(a8_vif)), p = fmt_p(a8_f$`Pr(>F)`[2]),
             check.names = FALSE))
a8_table
export_table(a8_table, "A8_table.tex")

# figure: the seats line before (A7) and after (A8, at the median distance)
# taking distance into account, each with its 95% confidence band
a8_grid <- data.frame(seats_10 = seq(0, max(a8$seats_10), length.out = 100),
                      distance_km = median(a8$distance_km))
a8_lines <- rbind(
  cbind(a8_grid, model = "A7: seats only",
        predict(a7_model, a8_grid, interval = "confidence")),
  cbind(a8_grid, model = "A8: seats + distance (at the median distance)",
        predict(a8_model, a8_grid, interval = "confidence")))

a8_plot <- ggplot(a8, aes(x = seats_inside, y = price_level)) +
  geom_point(alpha = 0.3) +
  geom_ribbon(data = a8_lines, aes(x = seats_10 * 10, y = fit, ymin = lwr, ymax = upr, fill = model),
              alpha = 0.2) +
  geom_line(data = a8_lines, aes(x = seats_10 * 10, y = fit, color = model), linewidth = 1) +
  labs(x = "Seats inside (number of seats)", y = "Price Level (CZK)", color = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
a8_plot
ggsave(file.path(dir_fig, "A8_figure.pdf"), a8_plot, width = 6, height = 4.5)

a8_ci <- coefci(a8_model, vcov = a8_vcov)["seats_10", ]
a8_ct <- coeftest(a8_model, vcov = a8_vcov)["seats_10", ]
add_summary("A8 Same, distance held fixed", "Multiple regression",
            paste(fmt(a8_ct[1]), "CZK", fmt_ci(a8_ci[1], a8_ci[2])),
            paste("R2 =", fmt(summary(a8_model)$r.squared)),
            paste0("t(", a8_model$df.residual, ") = ", fmt(a8_ct[3])),
            fmt_p(a8_ct[4]))


# ============================================================
# A9. INTERACTION: does the seats -> Price Level slope differ with / without outdoor seating?
# ============================================================
# H0: same slope in both groups (interaction = 0)   H1: the slopes differ (two-tailed)
a9 <- filter(df, !is.na(price_level), !is.na(seats_inside), !is.na(has_outdoor))
a9_lost <- nrow(df) - nrow(a9)
c(n = nrow(a9), lost = a9_lost)
a9$seats_10 <- a9$seats_inside / 10
a9$outdoor  <- factor(a9$has_outdoor, levels = c(0, 1),
                      labels = c("No outdoor seating", "With outdoor seating"))
table(a9$outdoor)   # cafes in each group

# seats_10 * outdoor = seats_10 + outdoor + seats_10:outdoor
# the last term (the interaction) is how much the slope changes with outdoor seating
a9_model    <- lm(price_level ~ seats_10 * outdoor, data = a9)
a9_additive <- lm(price_level ~ seats_10 + outdoor, data = a9)   # same slope in both groups
summary(a9_model)

# CHECKS: diagnostic plots, equal variance, influential cafes
pdf(file.path(dir_fig, "A9_diagnostics.pdf"), width = 10, height = 3.5)
par(mfrow = c(1, 3))
plot(a9_model, which = c(1, 2, 5))
dev.off()
bptest(a9_model)
a9_vcov <- choose_vcov(a9_model)

a9$cooks <- cooks.distance(a9_model)
a9 %>% arrange(desc(cooks)) %>% select(cafe, seats_inside, outdoor, price_level, cooks) %>% head(5)

# F-test of the interaction: does letting the slopes differ improve the model?
a9_f <- anova(a9_additive, a9_model)
a9_f

# the slope in EACH group, with its 95% CI (emmeans, as in 06_anova-family.R)
a9_slopes <- summary(emtrends(a9_model, ~ outdoor, var = "seats_10", vcov. = a9_vcov), infer = TRUE)
a9_slopes

a9_table <- rbind(
  regression_rows(a9_model, a9_vcov, "A9",
                  c("Intercept", "Seats inside (per 10)", "With outdoor seating",
                    "Seats (per 10) x outdoor")),
  data.frame(Model = "Slope by group",
             Term = paste("Seats (per 10):", a9_slopes$outdoor),
             Estimate = fmt(a9_slopes$seats_10.trend), SE = fmt(a9_slopes$SE),
             `95% CI` = fmt_ci(a9_slopes$lower.CL, a9_slopes$upper.CL),
             t = fmt(a9_slopes$t.ratio), p = fmt_p(a9_slopes$p.value),
             check.names = FALSE),
  data.frame(Model = "A9", Term = "Observations / R squared", Estimate = nrow(a9), SE = "",
             `95% CI` = paste("R2 =", fmt(summary(a9_model)$r.squared)), t = "", p = "",
             check.names = FALSE),
  data.frame(Model = "Interaction test", Term = "F-test", Estimate = "", SE = "",
             `95% CI` = paste0("F(", a9_f$Df[2], ", ", a9_f$Res.Df[2], ") = ", fmt(a9_f$F[2])),
             t = "", p = fmt_p(a9_f$`Pr(>F)`[2]),
             check.names = FALSE))
a9_table
export_table(a9_table, "A9_table.tex")

# figure: one fitted line (with its 95% band) per group, over the data
a9_plot <- ggplot(a9, aes(x = seats_inside, y = price_level, color = outdoor)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Seats inside (number of seats)", y = "Price Level (CZK)", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
a9_plot
ggsave(file.path(dir_fig, "A9_figure.pdf"), a9_plot, width = 6, height = 4.5)

a9_int <- which(rownames(coeftest(a9_model, vcov = a9_vcov)) == "seats_10:outdoorWith outdoor seating")
a9_ct  <- coeftest(a9_model, vcov = a9_vcov)[a9_int, ]
a9_ci  <- coefci(a9_model, vcov = a9_vcov)[a9_int, ]
add_summary("A9 Seats slope by outdoor seating", "Regression with interaction",
            paste(fmt(a9_ct[1]), "CZK", fmt_ci(a9_ci[1], a9_ci[2])),
            paste("R2 =", fmt(summary(a9_model)$r.squared)),
            paste0("F(", a9_f$Df[2], ", ", a9_f$Res.Df[2], ") = ", fmt(a9_f$F[2])),
            fmt_p(a9_f$`Pr(>F)`[2]))


# ============================================================
# A10. LOGISTIC REGRESSION: does the chance of an above-median Price Level change per 10 seats inside?
# ============================================================
# above_median_price = 1 if the Price Level is above the median (made in 01_cleaning.R)
# H0: the chance does not change with seats   H1: it changes (two-tailed)
a10 <- filter(df, !is.na(above_median_price), !is.na(seats_inside))
a10_lost <- nrow(df) - nrow(a10)
median(df$price_level, na.rm = TRUE)   # the median used for the cut: 79.7 CZK (a price: 1 decimal)
table(a10$above_median_price)          # events (1) and non-events (0)

# the logit model (05_logit-marginal-effects.R): family = binomial
a10_model <- glm(above_median_price ~ seats_inside, data = a10, family = binomial)
summary(a10_model)   # log-odds: only the sign and the p-value are readable

# the headline: average marginal effect of 10 MORE seats, in probability
# (multiply by 100 for percentage points)
a10_ame <- avg_comparisons(a10_model, variables = list(seats_inside = 10))
a10_ame

# the odds ratio per 10 seats, only as a check: exp(10 x coefficient)
a10_or    <- exp(10 * coef(a10_model)["seats_inside"])
a10_or_ci <- exp(10 * confint.default(a10_model)["seats_inside", ])
c(odds_ratio = unname(a10_or), lower = a10_or_ci[1], upper = a10_or_ci[2])

a10_table <- data.frame(
  n = nrow(a10), Lost = a10_lost, Events = sum(a10$above_median_price),
  `AME (pp)` = fmt(100 * a10_ame$estimate),           # per 10 seats
  `95% CI` = fmt_ci(100 * a10_ame$conf.low, 100 * a10_ame$conf.high),
  z = fmt(a10_ame$statistic),
  p = fmt_p(a10_ame$p.value),
  `Odds ratio [95% CI]` = paste(fmt(a10_or), fmt_ci(a10_or_ci[1], a10_or_ci[2])),   # per 10 seats
  check.names = FALSE)
a10_table
export_table(a10_table, "A10_table.tex")

# figure: the predicted probability by seats (with its 95% band), over the
# 0/1 data (jittered up and down a little so the dots don't overlap)
a10_plot <- plot_predictions(a10_model, condition = "seats_inside") +
  geom_point(data = a10, aes(x = seats_inside, y = above_median_price), inherit.aes = FALSE,
             position = position_jitter(width = 0, height = 0.03, seed = 123), alpha = 0.3) +
  labs(x = "Seats inside (number of seats)",
       y = "Probability of an above-median Price Level") +
  theme_minimal()
a10_plot
ggsave(file.path(dir_fig, "A10_figure.pdf"), a10_plot, width = 6, height = 4)

add_summary("A10 Above-median Price Level per 10 seats", "Logistic regression (AME)",
            paste(fmt(100 * a10_ame$estimate), "pp", fmt_ci(100 * a10_ame$conf.low, 100 * a10_ame$conf.high)),
            paste("OR =", fmt(a10_or)),
            paste("z =", fmt(a10_ame$statistic)),
            fmt_p(a10_ame$p.value))


# ============================================================
# SUMMARY TABLE OF ALL TEN ANALYSES (for the main text of the report)
# ============================================================
summary_rows
export_table(summary_rows, "summary.tex",
             align = c("l", "P{2.6cm}", "P{2.5cm}", "P{3.3cm}", "l", "P{2.3cm}", "l"))
