# 01_clean.R
# Load the raw survey, label every coded response (codes from RecSurveyInfo.pdf),
# and build the simplified variables used in the models.

source("R/00_setup.R")

raw <- read.csv(DATA_FILE)

# The file ends with one completely empty record; drop it.
raw <- raw[!is.na(raw$RecordNo) & !is.na(raw$wt), ]
message("Respondents: ", nrow(raw))

# ---- Non-response check ----------------------------------------------------
# Share of "Don't know" (8, 98) and "Prefer not to answer" (9, 99) answers per
# question; 999 marks a missing age.
nonresp <- raw |>
  summarise(across(-c(RecordNo, wt, OAGE, RESYR, NUMC, NUMA),
                   ~ mean(.x %in% c(8, 9, 98, 99, 999), na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "Question", values_to = "NonresponseShare") |>
  arrange(desc(NonresponseShare))
save_table(nonresp |> mutate(NonresponseShare = round(NonresponseShare, 3)),
           "nonresponse_by_question")

# ---- Label helpers -----------------------------------------------------------
lab4 <- function(x, labels, order_bad_to_good = TRUE) {
  f <- factor(x, levels = c(1, 2, 3, 4, 8, 9),
              labels = c(labels, "Don't know", "Prefer not to answer"))
  if (order_bad_to_good) f <- factor(f, levels = c(rev(labels), "Don't know", "Prefer not to answer"))
  f
}
rating <- c("Excellent", "Good", "Fair", "Poor")
likely <- c("Very likely", "Somewhat likely", "Somewhat unlikely", "Not at all likely")

df <- raw |>
  mutate(
    # Quality of life and parks (ordered bad -> good for plots)
    qol   = lab4(QoL, rating),
    park1 = lab4(PARK1, rating),
    park2 = lab4(PARK2, rating),

    # CC1: should Grand Junction build an indoor CRC?
    cc1 = lab4(CC1, c("Yes definitely", "Yes probably", "No probably not", "No definitely not")),

    # CC3: how much have you heard about the CRC?
    cc3 = lab4(CC3, c("A great deal", "A fair amount", "Not much", "Nothing at all")),

    # Randomized experiment 1: funding option shown (SELFUND) and support (CC4)
    selfund = factor(SELFUND, levels = 1:3,
                     labels = c("0.15% sales tax", "3-mill property tax", "15% nicotine tax")),
    cc4 = lab4(CC4, likely),

    cc5 = factor(CC5, levels = c(11, 12, 13, 98, 99),
                 labels = c("One large CRC", "Multiple smaller CRCs", "Do not support a CRC",
                            "Don't know", "Prefer not to answer")),
    cc6 = factor(CC6, levels = c(1, 2, 8, 9),
                 labels = c("Lincoln Park-Moyer Pool footprint", "Matchett Park",
                            "Don't know", "Prefer not to answer")),
    cc7 = factor(CC7, levels = c(11, 12, 13, 14, 98, 99),
                 labels = c("Sales tax", "Property tax", "Nicotine tax",
                            "Prefer not to build", "Don't know", "Prefer not to answer")),
    cc8 = lab4(CC8, c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree")),

    # Randomized experiment 2: admission price shown (SELPRICE) and expected use (CC9).
    # The codebook lists 5/6 for Don't know/Prefer not, but the data uses 8.
    selprice = factor(SELPRICE, levels = 1:4, labels = c("Free", "$4", "$8", "$12")),
    cc9 = factor(CC9, levels = c(4, 3, 2, 1, 8),
                 labels = c("Never", "Seldom", "Sometimes", "Frequently", "Don't know")),
    cc10 = lab4(CC10, likely),

    # Demographics
    oparty = factor(OPARTY, levels = c(1, 2, 4), labels = c("Republican", "Democrat", "Other")),
    educ = factor(EDUC, levels = c(1:6, 8, 9),
                  labels = c("Non high-school graduate", "High school or GED", "Some college",
                             "Associate/technical degree", "Bachelor's degree",
                             "Postgraduate degree", "Don't know", "Prefer not to answer")),
    gender = factor(GENDER, levels = c(1, 2, 3, 5, 9),
                    labels = c("Male", "Female", "Transgender", "Other identity",
                               "Prefer not to answer")),
    agerange = factor(AGERANGE, levels = 1:6,
                      labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")),
    income = factor(INCOME, levels = c(1:6, 8, 9),
                    labels = c("Under $25k", "$25k-$35k", "$35k-$50k", "$50k-$75k",
                               "$75k-$100k", "Over $100k", "Don't know", "Prefer not to answer")),
    fintoday = factor(FINTODAY, levels = c(2, 3, 1, 8, 9),
                      labels = c("Worse off", "About the same", "Better off",
                                 "Don't know", "Prefer not to answer")),
    finfut = factor(FINFUT, levels = c(2, 3, 1, 8, 9),
                    labels = c("Worse off", "About the same", "Better off",
                               "Don't know", "Prefer not to answer")),
    work = factor(WORK, levels = 1:9,
                  labels = c("Full-time", "Part-time", "Going to school", "Other",
                             "Unemployed looking for work", "Disabled", "Retired",
                             "Don't know", "Prefer not to answer")),
    hcm = factor(HCM, levels = c(1, 2, 8, 9), labels = c("Yes", "No", "Don't know", "Prefer not to answer")),

    # ---- Simplified variables for tables and models --------------------------
    cc1_simple = factor(case_when(CC1 %in% 1:2 ~ "Support", CC1 %in% 3:4 ~ "Oppose",
                                  CC1 %in% c(8, 9) ~ "Don't know / Prefer not"),
                        levels = c("Oppose", "Support", "Don't know / Prefer not")),
    support = case_when(CC1 %in% 1:2 ~ 1, CC1 %in% 3:4 ~ 0),   # binary outcome

    fintoday_3 = factor(case_when(FINTODAY == 2 ~ "Worse off", FINTODAY == 3 ~ "About the same",
                                  FINTODAY == 1 ~ "Better off", FINTODAY %in% c(8, 9) ~ "Unsure"),
                        levels = c("Worse off", "About the same", "Better off", "Unsure")),
    finfut_3 = factor(case_when(FINFUT == 2 ~ "Worse off", FINFUT == 3 ~ "About the same",
                                FINFUT == 1 ~ "Better off", FINFUT %in% c(8, 9) ~ "Unsure"),
                      levels = c("Worse off", "About the same", "Better off", "Unsure")),

    # Income in three bands, keeping non-response as its own group instead of
    # dropping 22% of the sample
    income_band = factor(case_when(INCOME %in% 1:3 ~ "Under $50k", INCOME %in% 4:5 ~ "$50k-$100k",
                                   INCOME == 6 ~ "Over $100k", INCOME %in% c(8, 9) ~ "Not reported"),
                         levels = c("Under $50k", "$50k-$100k", "Over $100k", "Not reported")),
    income_6 = factor(ifelse(INCOME %in% 1:6, INCOME, NA), levels = 1:6,
                      labels = levels(income)[1:6]),

    age_simple = factor(case_when(AGERANGE %in% 1:2 ~ "Under 35", AGERANGE %in% 3:4 ~ "35-54",
                                  AGERANGE %in% 5:6 ~ "55+"),
                        levels = c("35-54", "Under 35", "55+")),

    # NUMC values of 12, 22 and 32 are almost certainly entry errors; any
    # positive count still means children are in the household.
    children = factor(case_when(NUMC == 0 ~ "No children", NUMC > 0 & NUMC < 98 ~ "Children in household"),
                      levels = c("No children", "Children in household")),
    gender_2 = factor(case_when(GENDER == 1 ~ "Male", GENDER == 2 ~ "Female"),
                      levels = c("Male", "Female")),
    work_4 = factor(case_when(WORK %in% 1:2 ~ "Employed", WORK == 7 ~ "Retired",
                              WORK %in% 5:6 ~ "Not employed/disabled", WORK %in% 3:4 ~ "Student/other"),
                    levels = c("Employed", "Retired", "Not employed/disabled", "Student/other")),
    hcm_2 = factor(case_when(HCM == 1 ~ "Yes", HCM == 2 ~ "No"), levels = c("No", "Yes")),
    educ_3 = factor(reEDUC, levels = c(2, 4, 6),
                    labels = c("High school or less", "Some college", "College degree")),

    # Ordinal outcomes for the experiments (4 substantive levels, low -> high)
    cc4_ord = factor(ifelse(CC4 %in% 1:4, 5 - CC4, NA), levels = 1:4, labels = rev(likely), ordered = TRUE),
    cc9_ord = factor(ifelse(CC9 %in% 1:4, 5 - CC9, NA), levels = 1:4,
                     labels = c("Never", "Seldom", "Sometimes", "Frequently"), ordered = TRUE),
    cc4_likely = case_when(CC4 %in% 1:2 ~ 1, CC4 %in% 3:4 ~ 0),
    cc9_regular = case_when(CC9 %in% 1:2 ~ 1, CC9 %in% 3:4 ~ 0)   # frequently or sometimes
  )

saveRDS(df, CLEAN_FILE)
message("Saved ", CLEAN_FILE)
