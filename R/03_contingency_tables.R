# 03_contingency_tables.R
# Is support for the CRC (CC1) related to each predictor?
# For each table we report:
#   - the original unweighted Pearson chi-square (as in the first version), with
#     a flag when expected cell counts are too small for the test to be reliable
#   - the design-based Rao-Scott chi-square, which uses the survey weights and
#     accounts for the smaller effective sample size
#   - for ordered predictors, a design-based trend test: a weighted logistic
#     regression of support on the category number (1, 2, 3, ...). The plain
#     chi-square test ignores order; the trend test asks whether support rises
#     or falls steadily across the ordered groups.
#   - a Holm adjustment of the Rao-Scott p-values, because ten tests are run
# The outcome is Support vs. Oppose; the few Don't know / Prefer not answers
# (2.2%) are left out because they create cells with expected counts below 5.
# Script 06 rechecks the original three-column tables.

source("R/00_setup.R")
df <- readRDS(CLEAN_FILE) |> filter(!is.na(support))
df$support_f <- factor(df$support, levels = c(0, 1), labels = c("Oppose", "Support"))

predictors <- c(
  fintoday_3 = "Finances vs. a year ago", finfut_3 = "Expected finances next year",
  income_band = "Income", oparty = "Political party", agerange = "Age group",
  educ_3 = "Education", gender_2 = "Gender", children = "Children in household",
  hcm_2 = "Health club member", work_4 = "Work status"
)

# Ordered predictors and the levels used for the trend test (non-response
# categories such as "Unsure" or "Not reported" have no place in the order)
ordered_levels <- list(
  fintoday_3 = c("Worse off", "About the same", "Better off"),
  finfut_3 = c("Worse off", "About the same", "Better off"),
  income_band = c("Under $50k", "$50k-$100k", "Over $100k"),
  agerange = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  educ_3 = c("High school or less", "Some college", "College degree")
)
trend_test <- function(d, v) {
  lv <- ordered_levels[[v]]
  if (is.null(lv)) return(c(slope = NA, p = NA))
  dd <- d |> filter(.data[[v]] %in% lv) |> mutate(score = match(as.character(.data[[v]]), lv))
  m <- svyglm(support ~ score, design = make_design(dd), family = quasibinomial())
  c(slope = unname(coef(m)["score"]), p = coef(summary(m))["score", 4])
}

rows <- list()
for (v in names(predictors)) {
  d <- df |> filter(!is.na(.data[[v]])) |> droplevels()
  tab <- table(d[[v]], d$support_f)
  pear <- suppressWarnings(chisq.test(tab))
  min_exp <- min(pear$expected)
  fisher_p <- if (min_exp < 5) fisher.test(tab, simulate.p.value = TRUE, B = 1e5)$p.value else NA
  rs <- svychisq(as.formula(paste0("~", v, "+support_f")), make_design(d), statistic = "F")
  wtab <- svyby(~support, as.formula(paste0("~", v)), make_design(d), svymean)
  tr <- trend_test(d, v)
  rows[[v]] <- data.frame(
    Predictor = predictors[[v]],
    Levels = nlevels(d[[v]]),
    Pearson_chisq = unname(pear$statistic), Pearson_p = pear$p.value,
    Min_expected_count = min_exp, Fisher_p_if_sparse = fisher_p,
    RaoScott_F = unname(rs$statistic), RaoScott_p = rs$p.value,
    Trend_direction = ifelse(is.na(tr[["slope"]]), NA, ifelse(tr[["slope"]] > 0, "Rises", "Falls")),
    Trend_p_weighted = tr[["p"]],
    Weighted_support_range = sprintf("%.0f%% to %.0f%%", 100 * min(wtab$support), 100 * max(wtab$support))
  )
}
ct <- bind_rows(rows) |>
  mutate(RaoScott_p_Holm = p.adjust(RaoScott_p, method = "holm")) |>
  arrange(RaoScott_p)
rownames(ct) <- NULL
print(ct |> mutate(across(where(is.numeric), ~ signif(.x, 3))))
save_table(ct |> mutate(across(where(is.numeric), ~ signif(.x, 4))), "contingency_tests")

# Figure: weighted vs unweighted evidence
pd <- ct |> mutate(Predictor = factor(Predictor, levels = rev(Predictor))) |>
  pivot_longer(c(Pearson_p, RaoScott_p), names_to = "Test", values_to = "p") |>
  mutate(Test = recode(Test, Pearson_p = "Unweighted Pearson", RaoScott_p = "Weighted Rao-Scott"))
p <- ggplot(pd, aes(-log10(p), Predictor, color = Test)) +
  geom_vline(xintercept = -log10(0.05), linetype = "dashed", color = "grey40") +
  geom_point(size = 2.8) +
  annotate("text", x = -log10(0.05), y = 0.6, label = "p = 0.05", hjust = -0.1, size = 3) +
  scale_color_manual(values = c("grey60", "#2166AC"), name = NULL) +
  labs(title = "Which characteristics are related to CRC support?",
       subtitle = "Strength of evidence (further right = stronger)",
       x = expression(-log[10](p)), y = NULL)
save_plot(p, "05_contingency_evidence", w = 8, h = 4.8)
