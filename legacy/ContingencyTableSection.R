# Does Perecieved financial stability(past and presence) influence support for Com Center??

# let' use cc1 as main outcome and look are raw numbers for support
# Clearest choice since it ask directly if grand junction would support crc

df_eda <- df_eda |>
  mutate(cc1_simple = case_when(
    CC1 %in% c(1, 2) ~ "Support",
    CC1 %in% c(3, 4) ~ "Oppose",
    CC1 %in% c(8, 9) ~ "Don't know / Prefer not"
  ))

df_eda$cc1_simple <- factor(df_eda$cc1_simple,
                            levels = c("Oppose", "Support", "Don't know / Prefer not")
)


# Collapsin nonresponses for finfut and fintoday

df_ct <- df_eda |>
  mutate(
    fintoday_3 = case_when(
      fintoday %in% c("Worse off") ~ "Worse off",
      fintoday %in% c("About the same") ~ "About the same",
      fintoday %in% c("Better off") ~ "Better off",
      fintoday %in% c("Don't know", "Prefer not to answer") ~ "Unsure",
      TRUE ~ NA_character_
    ),
    
    finfut_3 = case_when(
      finfut %in% c("Worse off") ~ "Worse off",
      finfut %in% c("About the same") ~ "About the same",
      finfut %in% c("Better off") ~ "Better off",
      finfut %in% c("Don't know", "Prefer not to answer") ~ "Unsure",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    fintoday_3 = factor(fintoday_3,
                        levels = c("Worse off", "About the same", "Better off", "Unsure")),
    
    finfut_3 = factor(finfut_3,
                      levels = c("Worse off", "About the same", "Better off", "Unsure"))
  ) |>
  droplevels()

# financial perception of today compared to last year this time and support for crc


tab_fintoday_cc1 <- table(df_ct$fintoday_3, df_eda$cc1_simple)

tab_fintoday_cc1
prop.table(tab_fintoday_cc1, margin = 1)
chisq.test(tab_fintoday_cc1)

ggplot(df_ct, aes(x = fintoday_3, fill = cc1_simple)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Current Financial Situation",
    x = "Current financial situation",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal()

# Financial perception of the future and support for crc 

tab_finfut_cc1 <- table(df_ct$finfut_3, df_eda$cc1_simple)

tab_finfut_cc1
prop.table(tab_finfut_cc1, margin = 1)
chisq.test(tab_finfut_cc1)

ggplot(df_eda, aes(x = finfut, fill = cc1_simple)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Future Financial Outlook",
    x = "Future financial outlook",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal()



#looking at actual income and support for crc 

tab_income_cc1 <- table(df_eda$income, df_eda$cc1_simple)

tab_income_cc1
prop.table(tab_income_cc1, margin = 1)
chisq.test(tab_income_cc1)

ggplot(df_eda, aes(x = income, fill = cc1_simple)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Income",
    x = "Income",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# looking at actual income and current percieved financial status
df_eda <- df_eda |>
  mutate(fintoday_clean = case_when(
    fintoday %in% c("Don't know", "Prefer not to answer") ~ "Don't know/Prefer not to answer",
    TRUE ~ as.character(fintoday)
  ))

df_eda$fintoday_clean <- factor(df_eda$fintoday_clean,
                                levels = c("Worse off","About the same","Better off","Don't know/Prefer not to answer")
)

tab_income_fintoday <- table(df_eda$income, df_eda$fintoday_clean)

tab_income_fintoday
prop.table(tab_income_fintoday, margin = 1)
chisq.test(tab_income_fintoday)

    # combining prefer not to answer and dont know into nonresponse category for fintoday

df_eda <- df_eda |>
  mutate(fintoday_clean = case_when(
    fintoday %in% c("Don't know", "Prefer not to answer") ~ "Nonresponse",
    TRUE ~ as.character(fintoday)
  ))

df_eda$fintoday_clean <- factor(df_eda$fintoday_clean,
    levels = c("Worse off","About the same","Better off","Nonresponse")
)

df_eda <- df_eda |>
  mutate(finfut_clean = case_when(
    finfut %in% c("Don't know", "Prefer not to answer") ~ "Don't know/Prefer not to answer",
    TRUE ~ as.character(finfut)
  ))

df_eda$finfut_clean <- factor(df_eda$finfut_clean,
                                levels = c("Worse off","About the same","Better off","Don't know/Prefer not to answer")
)


# looking at actual income and percieved future financial situation

tab_income_finfut <- table(df_eda$income, df_eda$finfut_clean)

tab_income_finfut
prop.table(tab_income_finfut, margin = 1)
chisq.test(tab_income_finfut)




### POLITCAL AND DEMOGRAPHIC PREDICTORS- OTHER VARIABLES I WILL ADD TO LOGISTIC REGRESSION

# party and support 

tab_party_cc1 <- table(df_eda$oparty, df_eda$cc1_simple)

tab_party_cc1
prop.table(tab_party_cc1, margin = 1)
chisq.test(tab_party_cc1)

ggplot(df_eda, aes(x = oparty, fill = cc1_simple)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Political Party",
    x = "Political party",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal()
# age and support 

tab_age_cc1 <- table(df_eda$agerange, df_eda$cc1_simple)

tab_age_cc1
prop.table(tab_age_cc1, margin = 1)
chisq.test(tab_age_cc1)

# education and support

df_eda <- df_eda |>
  mutate(educ_clean = case_when(
    educ %in% c("Don't know", "Prefer not to answer") ~ "Nonresponse",
    TRUE ~ as.character(educ)
  ))

tab_educ_clean_cc1 <- table(df_eda$educ_clean, df_eda$cc1_simple)

tab_educ_clean_cc1
prop.table(tab_educ_clean_cc1, margin = 1)
chisq.test(tab_educ_clean_cc1)


# gender and support 
df_ct <- df_eda |>
  filter(!gender %in% c(
    "Prefer not to answer",
    "Do not identify as female, male, or transgender",
    "Transgender"
  )) |>
  droplevels()

tab_gender_cc1 <- table(df_ct$gender, df_ct$cc1_simple)
tab_gender_cc1
prop.table(tab_gender_cc1, margin = 1)
chisq.test(tab_gender_cc1)

### POLICY SPECIFIC TYPE VARIABLES- USING CC4 as outcome 

df_eda <- df_eda |>
  mutate(cc4_simple = case_when(
    CC4 %in% c(1, 2) ~ "Likely support funding",
    CC4 %in% c(3, 4) ~ "Unlikely support funding",
    CC4 %in% c(8, 9) ~ "Don't know / Prefer not"
  ))

df_eda$cc4_simple <- factor(df_eda$cc4_simple,
                            levels = c("Unlikely support funding", "Likely support funding", "Don't know / Prefer not")
)

# Funding type and funding support 

tab_selfund_cc4 <- table(df_eda$selfund, df_eda$cc4_simple)

tab_selfund_cc4
prop.table(tab_selfund_cc4, margin = 1)
chisq.test(tab_selfund_cc4)

# financial perception and funding support 

tab_fintoday_cc4 <- table(df_eda$fintoday, df_eda$cc4_simple)

tab_fintoday_cc4
prop.table(tab_fintoday_cc4, margin = 1)
chisq.test(tab_fintoday_cc4)

# children or not, and support 

df_ct <- df_eda |>
  mutate(
    children_simple = case_when(
      NUMC == 0 ~ "No children",
      NUMC > 0 & NUMC < 98 ~ "Children in household",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(children_simple)) |>
  droplevels()

tab_children_cc1 <- table(df_ct$children_simple, df_ct$cc1_simple)

tab_children_cc1
prop.table(tab_children_cc1, margin = 1)
chisq.test(tab_children_cc1)

# health club membership and support 

df_ct <- df_ct |>
  mutate(
    HCM_simple = case_when(
      HCM == 1 ~ "Yes",
      HCM == 2 ~ "No",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(HCM_simple)) |>
  droplevels()

tab_hcm_cc1 <- table(df_ct$HCM_simple, df_ct$cc1_simple)

tab_hcm_cc1
prop.table(tab_hcm_cc1, margin = 1)
chisq.test(tab_hcm_cc1)

# Work status and support 

table(df_eda$WORK, useNA = "ifany")

df_ct_work <- df_eda |>
  mutate(
    work_simple = case_when(
      WORK %in% c(1, 2) ~ "Employed",
      WORK == 7 ~ "Retired",
      WORK %in% c(5, 6) ~ "Not employed/disabled",
      WORK %in% c(3, 4) ~ "Student/other",
      WORK %in% c(8, 9) ~ NA_character_
    )
  ) |>
  filter(!is.na(work_simple)) |>
  droplevels()
table(df_ct_work$work_simple, useNA = "ifany")
nrow(df_ct_work)

tab_work_cc1 <- table(df_ct_work$work_simple, df_ct_work$cc1_simple)

tab_work_cc1
prop.table(tab_work_cc1, margin = 1)
chisq.test(tab_work_cc1)




### practical plots 

ggplot(df_eda, aes(x = fintoday, fill = cc1_simple)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Current Financial Situation",
    x = "Current financial situation",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal()

ggplot(df_eda, aes(x = finfut, fill = cc1_simple)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Future Financial Outlook",
    x = "Future financial outlook",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal()

ggplot(df_eda, aes(x = income, fill = cc1_simple)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Income",
    x = "Income",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_eda, aes(x = oparty, fill = cc1_simple)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Political Party",
    x = "Political party",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal()
## could use cc8 as well- do you think a crc would be a good use of city resources 

## also could use cc9- percieved likihood of crc use based on entrance fee 

