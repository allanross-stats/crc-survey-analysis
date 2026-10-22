# run_all.R
# Reproduces every table and figure from data/RecSurveyData.csv.
#   Rscript run_all.R

scripts <- c("R/01_clean.R", "R/02_weights_and_eda.R", "R/03_contingency_tables.R",
             "R/04_logistic_regression.R", "R/05_experiments.R",
             "R/06_test_assumptions.R")
for (s in scripts) {
  message("\n==== ", s, " ====")
  source(s, local = new.env())
}
message("\nDone. Tables in output/tables, figures in output/figures.")
