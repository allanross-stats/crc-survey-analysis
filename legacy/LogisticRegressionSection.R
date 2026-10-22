



########## FINAL FINAL 


# ---- Clean modeling dataset ----

df_model_clean <- df_eda |>
  mutate(
    # Outcome: 1 = support, 0 = oppose
    cc1_binary = case_when(
      CC1 %in% c(1, 2) ~ 1,
      CC1 %in% c(3, 4) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Current financial situation
    fintoday = factor(
      FINTODAY,
      levels = c(2, 3, 1),
      labels = c("Worse off", "About the same", "Better off"),
      ordered = FALSE
    ),
    
    # Future financial outlook
    finfut = factor(
      FINFUT,
      levels = c(2, 3, 1),
      labels = c("Worse off", "About the same", "Better off"),
      ordered = FALSE
    ),
    
    # Income: ordered low to high
    income = factor(
      INCOME,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c("Under $25k", "$25k-$35k", "$35k-$50k",
                 "$50k-$75k", "$75k-$100k", "Over $100k"),
      ordered = FALSE
    ),
  
    
    # Age groups collapsed
    age_simple = case_when(
      AGERANGE %in% c(1, 2) ~ "Young",
      AGERANGE %in% c(3, 4) ~ "Middle",
      AGERANGE %in% c(5, 6) ~ "Older",
      TRUE ~ NA_character_
    ),
    
    age_simple = factor(
      age_simple,
      levels = c("Middle", "Young", "Older")
    ),
    
    # Children in household
    children_simple = case_when(
      NUMC == 0 ~ "No children",
      NUMC > 0 & NUMC < 98 ~ "Children in household",
      NUMC %in% c(98, 99) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    children_simple = factor(
      children_simple,
      levels = c("No children", "Children in household")
    ),
    
    # Quality of life: ordered from low to high
    qol_simple = factor(
      QoL,
      levels = c(4, 3, 2, 1),
      labels = c("Poor", "Fair", "Good", "Excellent"),
      ordered = TRUE
    )
  ) |>
  filter(
    !is.na(cc1_binary),
    !is.na(fintoday),
    !is.na(finfut),
    !is.na(income),
    !is.na(age_simple),
    !is.na(children_simple),
    !is.na(qol_simple),
    !is.na(wt)
  ) |>
  droplevels()


##### FINAL FINAL MODEL WITH QOL

model_qol <- glm(
  cc1_binary ~ fintoday + finfut + income + age_simple +
    children_simple + qol_simple,
  data = df_model_clean,
  family = binomial,
  weights = wt
)

summary(model_qol)
AIC(model_qol)

##### WITHOUT QOL

model_final_clean <- glm(
  cc1_binary ~ fintoday + finfut + income + age_simple +
    children_simple,
  data = df_model_clean,
  family = binomial,
  weights = wt
)

summary(model_final_clean)

AIC(model_final_clean, model_qol)


##### more cleaning

df_model_final <- df_model_clean |>
  filter(
    # outcome
    !is.na(cc1_binary),
    
    # main predictors
    !fintoday %in% c("Don't know", "Prefer not to answer"),
    !finfut %in% c("Don't know", "Prefer not to answer"),
    
    # controls
    !income %in% c("Don't know", "Prefer not to answer"),
    !is.na(age_simple),
    !is.na(children_simple),
    
    # QoL (if included)
    !qol_simple %in% c("Don't know", "Prefer not to answer"),
    !is.na(qol_simple),
    
    # weights
    !is.na(wt)
  ) |>
  droplevels()


df_model_final <- df_model_clean |>
  filter(
    !is.na(cc1_binary),
    !is.na(fintoday),
    !is.na(finfut),
    !is.na(income),
    !is.na(age_simple),
    !is.na(children_simple),
    !is.na(qol_simple),
    !is.na(wt)
  ) |>
  droplevels()


### final final final


model_final <- glm(
  cc1_binary ~ fintoday + finfut + income + age_simple + children_simple,
  data = df_model_final,
  family = binomial,
  weights = wt
)

summary(model_final)
AIC(model_final)
