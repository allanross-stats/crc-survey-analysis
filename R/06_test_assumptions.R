# 06_test_assumptions.R
# Rechecks every chi-square test from the original ContingencyTableSection.R,
# using the same tables, and asks whether the chi-square test was appropriate.
#
# For each table:
#   - Rule of thumb (Cochran): the chi-square approximation is reliable when no
#     expected count is below 1 and no more than 20% of cells are below 5.
#   - If that fails: Fisher's exact test (Monte Carlo p-value for tables larger
#     than 2x2), which does not rely on the approximation.
#   - If the predictor is ordered (income, age, financial outlook): the
#     linear-by-linear association test, which uses the ordering and is more
#     powerful for detecting a trend than the plain chi-square test.

source("R/00_setup.R")
set.seed(1)
df <- readRDS(CLEAN_FILE)

# Recreate the original variables exactly as in ContingencyTableSection.R
df <- df |>
  mutate(
    educ_clean = ifelse(educ %in% c("Don't know", "Prefer not to answer"), "Nonresponse", as.character(educ)),
    fintoday_clean = factor(ifelse(fintoday %in% c("Don't know", "Prefer not to answer"),
                                   "Nonresponse", as.character(fintoday)),
                            levels = c("Worse off", "About the same", "Better off", "Nonresponse")),
    finfut_clean = factor(ifelse(finfut %in% c("Don't know", "Prefer not to answer"),
                                 "Nonresponse", as.character(finfut)),
                          levels = c("Worse off", "About the same", "Better off", "Nonresponse")),
    cc4_simple = factor(case_when(CC4 %in% 1:2 ~ "Likely", CC4 %in% 3:4 ~ "Unlikely",
                                  CC4 %in% c(8, 9) ~ "Don't know / Prefer not"),
                        levels = c("Unlikely", "Likely", "Don't know / Prefer not"))
  )

# Linear-by-linear association test (Agresti): M^2 = (N - 1) r^2 ~ chi-square(1),
# using equally spaced scores for the ordered categories of both variables.
lin_by_lin <- function(tab) {
  r <- row(tab); c <- col(tab); n <- sum(tab)
  rbar <- sum(r * tab) / n; cbar <- sum(c * tab) / n
  cov <- sum((r - rbar) * (c - cbar) * tab)
  rho <- cov / sqrt(sum((r - rbar)^2 * tab) * sum((c - cbar)^2 * tab))
  M2 <- (n - 1) * rho^2
  c(M2 = M2, p = pchisq(M2, 1, lower.tail = FALSE))
}

check <- function(name, x, y, ordered_rows = NULL, ordered_cols = NULL) {
  tab <- table(x, y)
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  ch <- suppressWarnings(chisq.test(tab))
  e <- ch$expected
  ok <- min(e) >= 1 && mean(e < 5) <= 0.2
  fisher_p <- if (!ok) {
    if (all(dim(tab) == 2)) fisher.test(tab)$p.value
    else fisher.test(tab, simulate.p.value = TRUE, B = 2e5)$p.value
  } else NA
  # Trend test only on the substantive (ordered) categories
  lbl <- NA
  if (!is.null(ordered_rows)) {
    keep_c <- if (is.null(ordered_cols)) intersect(c("Oppose", "Support"), colnames(tab)) else ordered_cols
    t2 <- tab[intersect(ordered_rows, rownames(tab)), keep_c, drop = FALSE]
    lbl <- lin_by_lin(t2)[["p"]]
  }
  data.frame(
    Table = name, Rows = nrow(tab), Cols = ncol(tab), N = sum(tab),
    Smallest_row_total = min(rowSums(tab)),
    Min_expected = round(min(e), 2),
    Pct_cells_expected_below_5 = round(100 * mean(e < 5)),
    Chisq_assumptions_met = ok,
    Chisq = round(unname(ch$statistic), 2), df = unname(ch$parameter),
    Chisq_p = signif(ch$p.value, 3),
    Fisher_p = signif(fisher_p, 3),
    Trend_test_p = signif(lbl, 3)
  )
}

fin_levels <- c("Worse off", "About the same", "Better off")
inc_levels <- c("Under $25k", "$25k-$35k", "$35k-$50k", "$50k-$75k", "$75k-$100k", "Over $100k")
age_levels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

g <- df |> filter(gender %in% c("Male", "Female")) |> droplevels()
k <- df |> filter(!is.na(children))
h <- k |> filter(HCM %in% 1:2) |> mutate(hcm_s = ifelse(HCM == 1, "Yes", "No"))
w <- df |> filter(!is.na(work_4))

res <- bind_rows(
  check("Finances vs. last year x CC1", df$fintoday_3, df$cc1_simple, fin_levels),
  check("Finances next year x CC1", df$finfut_3, df$cc1_simple, fin_levels),
  check("Income (8 levels) x CC1", df$income, df$cc1_simple, inc_levels),
  check("Income x finances vs. last year", df$income, df$fintoday_clean, inc_levels, fin_levels),
  check("Income x finances next year", df$income, df$finfut_clean, inc_levels, fin_levels),
  check("Party x CC1", df$oparty, df$cc1_simple),
  check("Age (6 groups) x CC1", df$agerange, df$cc1_simple, age_levels),
  check("Education x CC1", df$educ_clean, df$cc1_simple),
  check("Gender x CC1", g$gender, g$cc1_simple),
  check("Funding option x CC4", df$selfund, df$cc4_simple),
  check("Finances vs. last year (5 levels) x CC4", df$fintoday, df$cc4_simple),
  check("Children x CC1", k$children, k$cc1_simple),
  check("Health club x CC1", h$hcm_s, h$cc1_simple),
  check("Work status x CC1", w$work_4, w$cc1_simple)
)

print(res, row.names = FALSE)
save_table(res, "original_test_assumption_check")

# Holm adjustment: 14 tests were run, so some "significant" results could be
# chance. Holm controls the chance of any false positive across the set.
res$Best_p <- ifelse(res$Chisq_assumptions_met, res$Chisq_p, res$Fisher_p)
res$Holm_adjusted_p <- signif(p.adjust(res$Best_p, method = "holm"), 3)
save_table(res[, c("Table", "Best_p", "Holm_adjusted_p")], "original_tests_holm")
print(res[, c("Table", "Best_p", "Holm_adjusted_p")], row.names = FALSE)
