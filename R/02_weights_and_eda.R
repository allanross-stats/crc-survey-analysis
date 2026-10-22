# 02_weights_and_eda.R
# 1) What the survey weights do, and how much information they leave us with.
# 2) A from-scratch raking (iterative proportional fitting) demonstration.
# 3) Weighted vs. unweighted headline results.

source("R/00_setup.R")
df <- readRDS(CLEAN_FILE)
des <- make_design(df)

# ---- 1. Weight diagnostics -------------------------------------------------
w <- df$wt
kish_n <- sum(w)^2 / sum(w^2)       # effective sample size
weight_summary <- data.frame(
  Respondents = nrow(df),
  Effective_sample_size = round(kish_n),
  Design_effect = round(nrow(df) / kish_n, 2),
  Min_weight = min(w), Median_weight = round(median(w), 3), Max_weight = round(max(w), 3),
  Share_near_zero = round(mean(w < 0.01), 3)
)
print(weight_summary)
save_table(weight_summary, "weight_summary")

# Who was over- and under-represented? Compare unweighted vs weighted shares.
margin <- function(var) {
  u <- prop.table(table(df[[var]]))
  wtd <- prop.table(svytable(as.formula(paste0("~", var)), des))
  data.frame(Variable = var, Category = names(u),
             Unweighted = as.numeric(u), Weighted = as.numeric(wtd[names(u)]))
}
margins <- bind_rows(margin("agerange"), margin("oparty"), margin("gender_2"), margin("educ_3"))
save_table(margins |> mutate(across(c(Unweighted, Weighted), ~ round(.x, 3))), "weighted_vs_unweighted_demographics")

p <- margins |>
  pivot_longer(c(Unweighted, Weighted), names_to = "Sample", values_to = "Share") |>
  mutate(Category = factor(Category, levels = unique(margins$Category))) |>
  ggplot(aes(Category, Share, fill = Sample)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~Variable, scales = "free_x", nrow = 1,
             labeller = as_labeller(c(agerange = "Age", oparty = "Party",
                                      gender_2 = "Gender", educ_3 = "Education"))) +
  scale_fill_manual(values = c(Unweighted = "grey65", Weighted = "#2166AC"), name = NULL) +
  scale_y_continuous(labels = percent) +
  labs(title = "What the survey weights correct",
       subtitle = "Respondents 65+ were heavily over-represented; weighting shrinks their share",
       x = NULL, y = "Share of respondents") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_plot(p, "01_weights_demographics", w = 10, h = 4.5)

p <- ggplot(df, aes(wt)) +
  geom_histogram(bins = 50, fill = "#2166AC") +
  labs(title = "Distribution of survey weights",
       subtitle = sprintf("%d respondents carry the floor weight of %.4f; effective sample size = %d",
                          sum(w < 0.01), min(w), round(kish_n)),
       x = "Weight", y = "Respondents")
save_plot(p, "02_weight_distribution", w = 7, h = 4)

# ---- 2. Raking from scratch ------------------------------------------------
# Raking adjusts weights one variable at a time until the weighted sample
# matches known population shares for every variable at once. It does not
# change anyone's answers or resample anyone; it only changes how much each
# respondent counts. Population targets are not in the data file, so as a
# demonstration we rake the unweighted sample to the weighted age, party and
# gender margins and check how closely we can recover the official weights.
rake_by_hand <- function(data, vars, targets, iters = 50, tol = 1e-8) {
  wts <- rep(1, nrow(data))
  for (i in seq_len(iters)) {
    old <- wts
    for (v in vars) {
      cur <- tapply(wts, data[[v]], sum) / sum(wts)
      adj <- targets[[v]][names(cur)] / cur          # population share / sample share
      wts <- wts * adj[as.character(data[[v]])]
    }
    if (max(abs(wts - old)) < tol) break
  }
  list(weights = wts / mean(wts), iterations = i)
}
rk_data <- df |> filter(!is.na(agerange), !is.na(oparty), !is.na(gender_2))
rk_des <- make_design(rk_data)
targets <- lapply(c(agerange = "agerange", oparty = "oparty", gender_2 = "gender_2"),
                  function(v) { t <- svytable(as.formula(paste0("~", v)), rk_des); t / sum(t) })
rk <- rake_by_hand(rk_data, names(targets), targets)
rake_check <- data.frame(
  Iterations = rk$iterations,
  Correlation_with_official_weights = round(cor(rk$weights, rk_data$wt / mean(rk_data$wt)), 3)
)
print(rake_check)
save_table(rake_check, "raking_demo")

# ---- 3. Headline results, weighted vs unweighted ---------------------------
headline <- function(var, label) {
  u <- prop.table(table(df[[var]]))
  wtd <- svymean(as.formula(paste0("~", var)), des, na.rm = TRUE)
  ci <- confint(wtd)
  data.frame(Question = label, Response = sub(var, "", names(coef(wtd))),
             Unweighted = as.numeric(u[sub(var, "", names(coef(wtd)))]),
             Weighted = as.numeric(coef(wtd)), CI_low = ci[, 1], CI_high = ci[, 2])
}
heads <- bind_rows(headline("cc1_simple", "CC1: Build a CRC?"),
                   headline("cc5", "CC5: Preferred option"),
                   headline("cc6", "CC6: Preferred location"),
                   headline("cc7", "CC7: Preferred funding"))
save_table(heads |> mutate(across(where(is.numeric), ~ round(.x, 3))), "headline_results")
print(heads |> mutate(across(where(is.numeric), ~ round(.x, 3))))

# Figure: support (CC1, four levels), weighted with 95% CI
cc1_levels <- levels(df$cc1)[1:4]
d4 <- df |> filter(cc1 %in% cc1_levels) |> mutate(cc1 = droplevels(cc1))
s <- svymean(~cc1, make_design(d4))
cc1_df <- data.frame(Response = factor(cc1_levels, levels = cc1_levels),
                     Share = as.numeric(coef(s)), lo = confint(s)[, 1], hi = confint(s)[, 2])
p <- ggplot(cc1_df, aes(Response, Share, fill = Response)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15) +
  geom_text(aes(label = percent(Share, 1), y = hi + 0.03), size = 3.5) +
  scale_fill_manual(values = setNames(LIKERT4, cc1_levels), guide = "none") +
  scale_y_continuous(labels = percent, limits = c(0, 0.75)) +
  labs(title = "Should Grand Junction build an indoor Community Recreation Center?",
       subtitle = "Weighted share of respondents with an opinion; bars show 95% confidence intervals",
       x = NULL, y = NULL)
save_plot(p, "03_support_overall", w = 7.5, h = 4.5)

# Figure: support by group (weighted)
by_group <- function(var, label) {
  dd <- df |> filter(!is.na(.data[[var]]), !is.na(support))
  r <- svyby(~support, as.formula(paste0("~", var)), make_design(dd), svymean)
  data.frame(Group = label, Level = as.character(r[[var]]), Support = r$support,
             lo = r$support - 1.96 * r$se, hi = pmin(r$support + 1.96 * r$se, 1))
}
grp <- bind_rows(by_group("finfut_3", "Expected finances next year"),
                 by_group("fintoday_3", "Finances vs. a year ago"),
                 by_group("agerange", "Age"),
                 by_group("oparty", "Party"),
                 by_group("income_band", "Income"),
                 by_group("children", "Children at home"))
grp$Level <- factor(grp$Level, levels = rev(unique(grp$Level)))
grp$Group <- factor(grp$Group, levels = unique(grp$Group))
save_table(grp |> mutate(across(where(is.numeric), ~ round(.x, 3))), "support_by_group")
p <- ggplot(grp, aes(Support, Level)) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25, color = "grey50") +
  geom_point(color = "#2166AC", size = 2.4) +
  facet_grid(Group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(labels = percent, limits = c(0.45, 1)) +
  labs(title = "Support for building a CRC, by group",
       subtitle = "Weighted share saying yes (of those with an opinion), 95% CI",
       x = NULL, y = NULL) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"))
save_plot(p, "04_support_by_group", w = 8.5, h = 8)
