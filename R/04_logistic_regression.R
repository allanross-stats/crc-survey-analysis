# 04_logistic_regression.R
# Who supports building the CRC? (CC1: yes = 1, no = 0)
#
# Step 1 reproduces the original model exactly (glm with weights = wt).
# Step 2 refits the same model as a survey-weighted regression (svyglm), which
#        gives correct standard errors for weighted survey data.
# Step 3 fits the improved model: keeps income non-respondents, adds the
#        demographic controls the contingency tables flagged, and reports odds
#        ratios with confidence intervals and predicted probabilities.

source("R/00_setup.R")
df <- readRDS(CLEAN_FILE)

# ---- Step 1: original model ------------------------------------------------
orig_data <- df |>
  mutate(fintoday_m = factor(ifelse(FINTODAY %in% 1:3, as.character(fintoday), NA),
                             levels = c("Worse off", "About the same", "Better off")),
         finfut_m = factor(ifelse(FINFUT %in% 1:3, as.character(finfut), NA),
                           levels = c("Worse off", "About the same", "Better off"))) |>
  filter(!is.na(support), !is.na(fintoday_m), !is.na(finfut_m), !is.na(income_6),
         !is.na(age_simple), !is.na(children), QoL %in% 1:4) |>
  droplevels()

f_orig <- support ~ fintoday_m + finfut_m + income_6 + age_simple + children
m_glm <- suppressWarnings(glm(f_orig, data = orig_data, family = binomial, weights = wt))
message("Original model: n = ", nrow(orig_data), ", AIC = ", round(AIC(m_glm), 2))
print(round(coef(summary(m_glm)), 4))

# ---- Step 2: same model, survey-weighted ----------------------------------
m_svy_orig <- svyglm(f_orig, design = make_design(orig_data), family = quasibinomial())
se_compare <- data.frame(
  Term = names(coef(m_glm)),
  Estimate = coef(m_glm),
  Odds_ratio = exp(coef(m_glm)),
  SE_glm = sqrt(diag(vcov(m_glm))),
  SE_svyglm = sqrt(diag(vcov(m_svy_orig))),
  p_glm = coef(summary(m_glm))[, 4],
  p_svyglm = coef(summary(m_svy_orig))[, 4]
) |> mutate(SE_ratio = SE_svyglm / SE_glm)
print(se_compare |> mutate(across(where(is.numeric), ~ round(.x, 3))))
save_table(se_compare |> mutate(across(where(is.numeric), ~ round(.x, 4))), "original_model_glm_vs_svyglm")

# ---- Step 3: improved model ------------------------------------------------
mdata <- df |>
  filter(!is.na(support), !is.na(finfut_3), !is.na(fintoday_3), !is.na(income_band),
         !is.na(age_simple), !is.na(children), !is.na(oparty), !is.na(gender_2)) |>
  droplevels()
des <- make_design(mdata)
message("Improved model: n = ", nrow(mdata), " (original: ", nrow(orig_data), ")")

f_full <- support ~ finfut_3 + fintoday_3 + income_band + age_simple + children + oparty + gender_2
m_full <- svyglm(f_full, design = des, family = quasibinomial())

# Overall test for each variable (Wald F test, design-based)
terms <- attr(terms(f_full), "term.labels")
term_tests <- bind_rows(lapply(terms, function(t) {
  rt <- regTermTest(m_full, as.formula(paste("~", t)), method = "Wald")
  data.frame(Variable = t, F = as.numeric(rt$Ftest), df = rt$df, p = as.numeric(rt$p))
}))
print(term_tests |> mutate(across(where(is.numeric), ~ signif(.x, 3))))
save_table(term_tests |> mutate(across(where(is.numeric), ~ signif(.x, 4))), "improved_model_term_tests")

# Odds ratios with 95% confidence intervals
ci <- confint(m_full)
or_tab <- data.frame(Term = names(coef(m_full)), Estimate = coef(m_full),
                     Odds_ratio = exp(coef(m_full)), CI_low = exp(ci[, 1]), CI_high = exp(ci[, 2]),
                     p = coef(summary(m_full))[, 4]) |> filter(Term != "(Intercept)")
pretty_term <- function(t) {
  t |> sub("^finfut_3", "Next year: ", x = _) |> sub("^fintoday_3", "Vs. last year: ", x = _) |>
    sub("^income_band", "Income: ", x = _) |> sub("^age_simple", "Age: ", x = _) |>
    sub("^children", "", x = _) |> sub("^oparty", "Party: ", x = _) |> sub("^gender_2", "", x = _)
}
or_tab$Label <- pretty_term(or_tab$Term)
print(or_tab |> mutate(across(where(is.numeric), ~ round(.x, 3))))
save_table(or_tab |> mutate(across(where(is.numeric), ~ round(.x, 4))), "improved_model_odds_ratios")

group_of <- function(t) case_when(grepl("^finfut", t) ~ "Expected finances (ref: worse off)",
                                  grepl("^fintoday", t) ~ "Finances vs. last year (ref: worse off)",
                                  grepl("^income", t) ~ "Income (ref: under $50k)",
                                  grepl("^age", t) ~ "Age (ref: 35-54)",
                                  grepl("^oparty", t) ~ "Party (ref: Republican)",
                                  TRUE ~ "Household / gender")
or_tab$Group <- factor(group_of(or_tab$Term), levels = unique(group_of(or_tab$Term)))
p <- ggplot(or_tab, aes(Odds_ratio, reorder(Label, -as.numeric(Group)))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2, color = "grey45") +
  geom_point(aes(color = p < 0.05), size = 2.6) +
  facet_grid(Group ~ ., scales = "free_y", space = "free_y") +
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 8, 16)) +
  scale_color_manual(values = c(`TRUE` = "#2166AC", `FALSE` = "grey60"),
                     labels = c(`TRUE` = "p < 0.05", `FALSE` = "not significant"), name = NULL) +
  labs(title = "What predicts support for the CRC?",
       subtitle = "Odds ratios from a survey-weighted logistic regression, with 95% confidence intervals (log scale)",
       x = "Odds ratio (right of 1 = more likely to support)", y = NULL) +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"))
save_plot(p, "06_odds_ratios", w = 9.5, h = 6.5)

# ---- Predicted probabilities ----------------------------------------------
# Average predicted support if everyone had a given expected-finances answer,
# keeping all their other characteristics as they are (weighted).
avg_pred <- function(var, lvls) {
  bind_rows(lapply(lvls, function(l) {
    nd <- mdata; nd[[var]] <- factor(l, levels = levels(mdata[[var]]))
    data.frame(Variable = var, Level = l,
               Predicted_support = weighted.mean(predict(m_full, newdata = nd, type = "response"), mdata$wt))
  }))
}
preds <- bind_rows(avg_pred("finfut_3", levels(mdata$finfut_3)),
                   avg_pred("age_simple", c("Under 35", "35-54", "55+")),
                   avg_pred("children", levels(mdata$children)))
print(preds |> mutate(Predicted_support = round(Predicted_support, 3)))
save_table(preds |> mutate(Predicted_support = round(Predicted_support, 4)), "predicted_support")

# Two example residents
profiles <- data.frame(
  Profile = c("65+ Republican, no kids, expects to be worse off",
              "35-54, kids at home, expects to be better off"),
  finfut_3 = c("Worse off", "Better off"), fintoday_3 = c("About the same", "About the same"),
  income_band = c("$50k-$100k", "$50k-$100k"), age_simple = c("55+", "35-54"),
  children = c("No children", "Children in household"), oparty = c("Republican", "Republican"),
  gender_2 = c("Male", "Male"))
for (v in names(profiles)[-1]) profiles[[v]] <- factor(profiles[[v]], levels = levels(mdata[[v]]))
profiles$Predicted_support <- as.numeric(predict(m_full, newdata = profiles, type = "response"))
print(profiles[, c("Profile", "Predicted_support")])
save_table(profiles |> mutate(Predicted_support = round(Predicted_support, 3)), "example_profiles")

# ---- Diagnostics -----------------------------------------------------------
# 1. Are the two financial questions measuring the same thing? (weighted Cramer's V)
tab <- svytable(~fintoday_3 + finfut_3, des)
chi <- suppressWarnings(chisq.test(tab * nrow(mdata) / sum(tab)))
cramers_v <- sqrt(as.numeric(chi$statistic) / (nrow(mdata) * (min(dim(tab)) - 1)))

# 2. Does removing "finances vs. last year" change the fit much?
m_nofin <- update(m_full, . ~ . - fintoday_3)

# 3. Discrimination: area under the ROC curve (weighted)
p_hat <- fitted(m_full)
auc_w <- {
  y <- mdata$support; w <- mdata$wt
  pos <- which(y == 1); neg <- which(y == 0)
  cmp <- outer(p_hat[pos], p_hat[neg], function(a, b) (a > b) + 0.5 * (a == b))
  sum(cmp * outer(w[pos], w[neg])) / (sum(w[pos]) * sum(w[neg]))
}
diag_tab <- data.frame(
  n = nrow(mdata),
  Effective_n = round(sum(mdata$wt)^2 / sum(mdata$wt^2)),
  CramersV_fintoday_finfut = round(cramers_v, 3),
  Weighted_AUC = round(auc_w, 3),
  AIC_full = round(AIC(m_full)[["AIC"]], 1),
  AIC_without_fintoday = round(AIC(m_nofin)[["AIC"]], 1)
)
print(diag_tab)
save_table(diag_tab, "model_diagnostics")
saveRDS(list(m_full = m_full, mdata = mdata), "data/logit_model.rds")
