# 00_setup.R
# Packages, paths and plot style shared by every script.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(survey)   # design-based (weighted) estimates, svyglm, svychisq, rake
})
options(survey.lonely.psu = "adjust", dplyr.summarise.inform = FALSE)

DATA_FILE <- "data/RecSurveyData.csv"
CLEAN_FILE <- "data/clean.rds"
FIG_DIR <- "output/figures"
TAB_DIR <- "output/tables"
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

SUPPORT_COLORS <- c("Oppose" = "#D6604D", "Support" = "#4393C3",
                    "Don't know / Prefer not" = "#BABABA")
LIKERT4 <- c("#B2182B", "#EF8A62", "#67A9CF", "#2166AC")

theme_set(theme_minimal(base_size = 11) +
            theme(plot.title = element_text(face = "bold"),
                  panel.grid.minor = element_blank(),
                  legend.position = "bottom"))

save_plot <- function(p, name, w = 8, h = 4.5) {
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), p, width = w, height = h,
         dpi = 150, bg = "white")
}
save_table <- function(df, name) {
  write.csv(df, file.path(TAB_DIR, paste0(name, ".csv")), row.names = FALSE)
}

# Survey design: the CMU Social Research Center raked the sample so its
# demographics match Grand Junction; `wt` holds those weights. No strata or
# clusters were provided, so this is a weighted simple random sample design.
make_design <- function(df) svydesign(ids = ~1, weights = ~wt, data = df)
