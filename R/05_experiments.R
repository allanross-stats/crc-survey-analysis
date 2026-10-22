# 05_experiments.R
# The survey contains two randomized experiments. Each respondent was randomly
# shown ONE funding source (SELFUND) before CC4 and ONE admission price
# (SELPRICE) before CC9. Because the version shown was random, differences in
# answers between versions are caused by the version itself, not by who the
# respondents were.
#
# For each experiment:
#   1. Balance check: were the randomized groups similar in age, party, etc.?
#   2. Effect: weighted share giving a positive answer, by version
#   3. Proportional-odds (ordinal logistic) model using all four answer levels
#   4. Does the effect differ by political party?

source("R/00_setup.R")
df <- readRDS(CLEAN_FILE)

balance_check <- function(d, arm) {
  covs <- c(agerange = "Age", oparty = "Party", gender_2 = "Gender",
            income_band = "Income", children = "Children")
  bind_rows(lapply(names(covs), function(v) {
    dd <- d |> filter(!is.na(.data[[v]]), !is.na(.data[[arm]])) |> droplevels()
    t <- suppressWarnings(chisq.test(table(dd[[v]], dd[[arm]])))
    data.frame(Covariate = covs[[v]], Chisq = unname(t$statistic), df = unname(t$parameter), p = t$p.value)
  }))
}

share_by_arm <- function(d, outcome, arm) {
  dd <- d |> filter(!is.na(.data[[outcome]]), !is.na(.data[[arm]]))
  r <- svyby(as.formula(paste0("~", outcome)), as.formula(paste0("~", arm)), make_design(dd), svymean)
  data.frame(Arm = r[[arm]], Share = r[[outcome]], SE = r$se,
             lo = r[[outcome]] - 1.96 * r$se, hi = r[[outcome]] + 1.96 * r$se,
             n = as.numeric(table(dd[[arm]])[as.character(r[[arm]])]))
}

ordinal_model <- function(d, outcome, arm, covars = NULL) {
  dd <- d |> filter(!is.na(.data[[outcome]]), !is.na(.data[[arm]]))
  if (length(covars)) dd <- dd |> filter(if_all(all_of(covars), ~ !is.na(.x)))
  dd <- droplevels(dd)
  f <- as.formula(paste(outcome, "~", paste(c(arm, covars), collapse = " + ")))
  m <- svyolr(f, design = make_design(dd))
  est <- coef(m); se <- sqrt(diag(vcov(m)))[names(est)]
  keep <- grepl(paste0("^", arm), names(est))
  data.frame(Term = names(est)[keep], Log_odds = est[keep], SE = se[keep],
             Odds_ratio = exp(est[keep]), CI_low = exp(est[keep] - 1.96 * se[keep]),
             CI_high = exp(est[keep] + 1.96 * se[keep]),
             p = 2 * pnorm(-abs(est[keep] / se[keep])), n = nrow(dd))
}

# =========================================================================
# Experiment 1: funding source -> support (CC4)
# =========================================================================
bal1 <- balance_check(df, "selfund")
fund_share <- share_by_arm(df, "cc4_likely", "selfund")
fund_ord <- ordinal_model(df, "cc4_ord", "selfund")
fund_ord_adj <- ordinal_model(df, "cc4_ord", "selfund", c("oparty", "age_simple", "gender_2"))

# Does the effect of the funding source depend on party?
d1 <- df |> filter(!is.na(cc4_likely), !is.na(selfund), !is.na(oparty))
fund_party <- svyby(~cc4_likely, ~selfund + oparty, make_design(d1), svymean)
fund_party <- data.frame(Funding = fund_party$selfund, Party = fund_party$oparty,
                         Share = fund_party$cc4_likely, SE = fund_party$se)
m_int <- svyglm(cc4_likely ~ selfund * oparty, design = make_design(d1), family = quasibinomial())
int_test <- regTermTest(m_int, ~ selfund:oparty)

# =========================================================================
# Experiment 2: admission price -> expected use (CC9)
# =========================================================================
# Note: respondents who said they do not support a CRC at all (CC5 = 13) were
# mostly not asked CC9, so this experiment describes likely users.
bal2 <- balance_check(df |> filter(!is.na(selprice)), "selprice")
price_share <- share_by_arm(df, "cc9_regular", "selprice")
price_ord <- ordinal_model(df, "cc9_ord", "selprice")
price_ord_adj <- ordinal_model(df, "cc9_ord", "selprice", c("oparty", "age_simple", "gender_2"))

# Price as a number: change in odds of more frequent use per $1
dprice <- df |> filter(!is.na(cc9_ord), !is.na(selprice)) |>
  mutate(price_usd = c(0, 4, 8, 12)[as.integer(selprice)])
m_lin <- svyolr(cc9_ord ~ price_usd, design = make_design(dprice))
per_dollar <- data.frame(Term = "Per $1 increase", Odds_ratio = exp(coef(m_lin)[["price_usd"]]),
                         CI_low = exp(coef(m_lin)[["price_usd"]] - 1.96 * sqrt(vcov(m_lin)["price_usd", "price_usd"])),
                         CI_high = exp(coef(m_lin)[["price_usd"]] + 1.96 * sqrt(vcov(m_lin)["price_usd", "price_usd"])))

# ---- Save tables -----------------------------------------------------------
r3 <- function(x) x |> mutate(across(where(is.numeric), ~ round(.x, 4)))
save_table(r3(bind_rows(bal1 |> mutate(Experiment = "Funding"), bal2 |> mutate(Experiment = "Price"))), "experiment_balance")
save_table(r3(bind_rows(fund_share |> mutate(Experiment = "Funding -> likely to support (CC4)"),
                        price_share |> mutate(Experiment = "Price -> use frequently/sometimes (CC9)"))), "experiment_effects")
save_table(r3(bind_rows(fund_ord |> mutate(Model = "Funding, unadjusted"),
                        fund_ord_adj |> mutate(Model = "Funding, adjusted"),
                        price_ord |> mutate(Model = "Price, unadjusted"),
                        price_ord_adj |> mutate(Model = "Price, adjusted"))), "experiment_ordinal_models")
save_table(r3(fund_party), "funding_by_party")
save_table(r3(per_dollar), "price_per_dollar")

cat("\nBalance:\n"); print(r3(bal1)); print(r3(bal2))
cat("\nFunding effects:\n"); print(r3(fund_share)); print(r3(fund_ord)); print(r3(fund_ord_adj))
cat("\nParty interaction p =", signif(as.numeric(int_test$p), 3), "\n"); print(r3(fund_party))
cat("\nPrice effects:\n"); print(r3(price_share)); print(r3(price_ord)); print(r3(per_dollar))

# ---- Figures ---------------------------------------------------------------
p <- ggplot(fund_share, aes(Arm, Share)) +
  geom_col(fill = "#4393C3", width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12) +
  geom_text(aes(label = percent(Share, 1), y = hi + 0.04), size = 3.6) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Experiment 1: How the funding source changes support",
       subtitle = "Weighted share likely to support, by randomly assigned funding source (95% CI)",
       x = NULL, y = NULL)
save_plot(p, "07_funding_experiment", w = 7.5, h = 4.5)

fp <- fund_party |> mutate(lo = Share - 1.96 * SE, hi = pmin(Share + 1.96 * SE, 1))
p <- ggplot(fp, aes(Funding, Share, fill = Party)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(0.75), width = 0.15) +
  scale_fill_manual(values = c(Republican = "#D6604D", Democrat = "#4393C3", Other = "#A6A6A6"), name = NULL) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Funding experiment by political party",
       subtitle = "Weighted share likely to support, by funding source shown and party (95% CI)",
       x = NULL, y = NULL)
save_plot(p, "08_funding_by_party", w = 8, h = 4.5)

p <- ggplot(price_share, aes(Arm, Share, group = 1)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#4393C3", alpha = 0.2) +
  geom_line(color = "#2166AC", linewidth = 1) +
  geom_point(color = "#2166AC", size = 3) +
  geom_text(aes(label = percent(Share, 1)), vjust = -1.2, size = 3.6) +
  scale_y_continuous(labels = percent, limits = c(0.4, 1)) +
  labs(title = "Experiment 2: How the admission price changes expected use",
       subtitle = "Weighted share who would use it frequently or sometimes (95% band)",
       x = "Adult daily admission price shown", y = NULL)
save_plot(p, "09_price_experiment", w = 7.5, h = 4.5)
