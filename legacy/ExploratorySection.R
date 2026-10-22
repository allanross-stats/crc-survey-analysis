## Set Directory and calling raw data 
install.packages("here")
library(here)
rawData <- read.csv(here("Desktop", "Categorical", "RecSurveyData.csv"))
View(rawData)

## Load and Standardise data- looking at raw data, 

install.packages(c("tidyverse", "janitor"))
library(tidyverse)
library(janitor)
glimpse(rawData) # everything except gender and weight is coded as an integer, 
                 # gender is coded as a character, weight is coded as a double 
summary(rawData)

###dealing with NA's, "dont know", "prefer not to answer"


rawData |> summarise(across(everything(), ~ sum(.x %in% c(8,9,98,99,999), na.rm = TRUE))) 

# this counts proportions of "dont know" and "prefer not to answer" per row 
# significant proportion : park2, cc6, race_A, income, finfut
rawData |> summarise(across(everything(), ~mean(.x %in% c(8,9,98,99,999), na.rm = TRUE))) * 100

# this gives proportion's "dont know"s 8's and 98's per row
# significant proportion: park2, cc6
rawData |> summarise(across(everything(), ~mean(.x %in% c(8,98), na.rm = TRUE))) * 100

# this gives proportion's "prefer not to answer"s 9's and 99's per row
# significant proportion: income
rawData |> summarise(across(everything(), ~mean(.x %in% c(9,99), na.rm = TRUE))) * 100

# this gives proportion's of 999's- in the AGE column- if they did not answer- "what was age last birthday"- response coded 999
rawData |> summarise(across(everything(), ~mean(.x %in% c(999), na.rm = TRUE))) * 100
# 7 percent of age consists of responses with 999- meaning they did not answer the question
# imported age is another variable that can be used to track age 

# there aren't many 999's- lets look for it--- there is a good amount of them
# might be better to use OAGE to keep track of age 
rawData |> filter(AGE== 999)

### Factorizing ordered data for readibility 

df_eda <- rawData

### HOW WOULD YOU RATE GRAND JUNCTION AS A PLACE TO LIVE? QoL
df_eda$qol <- factor(df_eda$QoL,
                     levels = c(1,2,3,4,8,9),
                     labels = c("Excellent","Good","Fair","Poor","Don't know","Prefer not to answer"))

# I want the categories to display from bad to good 
df_eda$qol <- factor(
  df_eda$qol,
  levels = c("Poor","Fair","Good","Excellent","Don't know","Prefer not to answer"))

ggplot(df_eda, aes(x = qol)) + geom_bar()

### HOW WOULD YOU RATE THE CURRENT OVERALL PARKS/ REC SYSTEM IN GRAND JUNCTION? PARK1

df_eda$park1 <- factor(df_eda$PARK1,
                     levels = c(1,2,3,4,8,9),
                     labels = c("Excellent","Good","Fair","Poor","Don't know","Prefer not to answer"))

df_eda$park1 <- factor(
  df_eda$park1,
  levels = c("Poor","Fair","Good","Excellent","Don't know","Prefer not to answer"))

ggplot(df_eda, aes(x = park1)) + geom_bar()

### HOW WOULD YOU RATE THE CURRENT RECREATION FACILITIES IN GRAND JUNCTION? PARK2

df_eda$park2 <- factor(df_eda$PARK2,
                       levels = c(1,2,3,4,8,9),
                       labels = c("Excellent","Good","Fair","Poor","Don't know","Prefer not to answer"))

df_eda$park2 <- factor(
  df_eda$park2,
  levels = c("Poor","Fair","Good","Excellent","Don't know","Prefer not to answer"))

ggplot(df_eda, aes(x = park2)) + geom_bar()

### There have been on-going discussions about whether the City of Grand Junction should build an indoor 
###Community Recreation Center for its residents. Based on what you know, do you think Grand Junction should
### build an indoor Community Recreation Center for Grand Junction residents, or not? CC1

df_eda$cc1 <- factor(df_eda$CC1,
                     levels = c(1,2,3,4,8,9),
                     labels = c("Yes definitely","Yes probably","No probably not","No definitely not",
                                "Don't know","Prefer not to answer"))
df_eda$cc1 <- factor(
  df_eda$cc1,
  levels = c("No definitely not","No probably not","Yes probably","Yes definitely",
             "Don't know","Prefer not to answer"))
ggplot(df_eda, aes(x = cc1)) + geom_bar()

### How much have you read, seen, or heard about the possible development of a Community Recreation 
##Center by the City of Grand Junction? CC3

df_eda$cc3 <- factor(df_eda$CC3,
                     levels = c(1,2,3,4,8,9),
                     label = c("A great deal", "A fair amount", "Not much",
                               "Nothing at all", "Do not know", "prefer not to answer"))
df_eda$cc3 <- factor(
  df_eda$cc3, 
  levels = c("Nothing at all", "Not much", "A fair amount", "A great deal",
             "Do not know", "prefer not to answer"))
ggplot(df_eda, aes(x = cc3)) + geom_bar()

### SELFUND: 1- sales tax, 2- property tax, 3 - nicotine tax-- RANDOM FUND OPTION

### Although voters in Grand Junction legalized the sale, cultivation, and taxation of cannabis in city limits to
##Recreation Center. As such, the city would require additional revenues to fully fund the construction of a
##Community Recreation Center. How likely are you to support construction of an indoor Community Recreation
##Center that is funded by [RANDOM FUND OPTION]? CC4

df_eda$selfund <- factor(df_eda$SELFUND,
                         levels = c(1,2,3),
                         labels = c("Sales tax","Property tax","Nicotine tax"))

df_eda$cc4 <- factor(df_eda$CC4,
                     levels = c(1,2,3,4,8,9),
                     labels = c("Very likely","Somewhat likely","Somewhat unlikely","Not at all likely",
                                "Don't know","Prefer not to answer"))
df_eda$cc4 <- factor(
  df_eda$cc4,
  levels = c("Not at all likely","Somewhat unlikely","Somewhat likely","Very likely",
             "Don't know","Prefer not to answer"))

ggplot(df_eda, aes(x = cc4, fill = selfund)) + geom_bar() ### fix x axis names(too close togther) + add counts to each fund choice per category 

### CC5. If the necessary funding to build a Community Recreation Center was secured, which option would you
##prefer: Option A: Build a large Community Recreation Center in a single location including such components as: large
##warm water leisure pool, large lap pool, outdoor pool, therapy pool, large fitness/weight area, large walk/jog
##track and a large 4-6 court gymnasium, among other amenities.
##Option B: Build multiple small Community Recreation Centers in different locations each with components
##such as a small warm water leisure OR lap pool, small fitness/weights area, small walk/jog track and a small 1-2
##court gymnasium, among other amenities.
##Option C: I do not support pursuit of a Community Recreation Center/s. CC5 

df_eda$cc5 <- factor(df_eda$CC5,
                     levels = c(11,12,13,98,99),
                     labels = c("Option A", "Option B", "Option C", "Don't know", "prefer not answer"))
ggplot(df_eda, aes(x = cc5, fill = cc1)) + geom_bar() ### Looking at counts of cc1 and cc5 together 
#people that said yes definitely liked the idea of single large rec center, 
#could compare to other data as well, cc4, cc2, park1, park2

df_eda |>
  count(cc5) |>
  mutate(prop = n / sum(n))
# this shows the spread of answers by proportion

###CC6. Regardless of your answer to the last question, if a large indoor Community Recreation Center was built
##including both indoor and outdoor pools, would you prefer that it be built on the footprint of the existing
##Lincoln Park-Moyer Outdoor Pool (the rest of the park and the golf course would be unaffected) or in Matchett
##Park at the center of the undeveloped site? CC6

df_eda$cc6 <- factor(df_eda$CC6,
                     levels = c(1,2,8,9),
                     labels = c("Footprint of Lincoln-Park Pool", 
                                "Matchett park", 
                                "Do not know", 
                                "Prefer not to answer"))
ggplot(df_eda, aes(x = cc6, fill = cc5)) + geom_bar()
# compares cc5 and cc6, people who prefer one facility want it on the footprint of matchett park
# matchett park is where the facility is actually being built 

df_eda |>
  count(cc6) |>
  mutate(prop = n / sum(n)) # proportions of answers 


df_eda$cc7 <- factor(df_eda$CC7,
                     levels = c(11,12,13,14,98,99),
                     labels = c("Sales tax","Property tax","Nicotine tax",
                                "Prefer not to build CRC","Don't know","Prefer not to answer")
)

df_eda$cc8 <- factor(df_eda$CC8,
                     levels = c(1,2,3,4,8,9),
                     labels = c("Strongly agree","Somewhat agree","Somewhat disagree",
                                "Strongly disagree","Don't know","Prefer not to answer")
)

df_eda$cc8 <- factor(df_eda$cc8,
                     levels = c("Strongly disagree","Somewhat disagree",
                                "Somewhat agree","Strongly agree",
                                "Don't know","Prefer not to answer")
)

df_eda$selprice <- factor(df_eda$SELPRICE,
                          levels = c(1,2,3,4),
                          labels = c("Free","$4","$8","$12")
)

df_eda$cc9 <- factor(df_eda$CC9,
                     levels = c(1,2,3,4,5,6),
                     labels = c("Frequently","Sometimes","Seldom","Never",
                                "Don't know","Prefer not to answer")
)

df_eda$cc9 <- factor(df_eda$cc9,
                     levels = c("Never","Seldom","Sometimes","Frequently",
                                "Don't know","Prefer not to answer")
)

df_eda$cc10 <- factor(df_eda$CC10,
                      levels = c(1,2,3,4,8,9),
                      labels = c("Very likely","Somewhat likely","Somewhat unlikely",
                                 "Not at all likely","Don't know","Prefer not to answer")
)

df_eda$cc10 <- factor(df_eda$cc10,
                      levels = c("Not at all likely","Somewhat unlikely",
                                 "Somewhat likely","Very likely",
                                 "Don't know","Prefer not to answer")
)

## Recoding Demographics 


df_eda$oparty <- factor(df_eda$OPARTY,
                        levels = c(1,2,4),
                        labels = c("Republican","Democrat","Other")
)

df_eda$educ <- factor(df_eda$EDUC,
                      levels = c(1,2,3,4,5,6,8,9),
                      labels = c("Non high-school graduate","High school or GED",
                                 "Some college","Associate/technical degree",
                                 "Bachelor's degree","Postgraduate degree",
                                 "Don't know","Prefer not to answer")
)

df_eda$mar <- factor(df_eda$MAR,
                     levels = c(1,2,3,4,5,6,9),
                     labels = c("Single, never married","Married","Separated",
                                "Divorced","Widowed","Don't know",
                                "Prefer not to answer")
)

df_eda$hcm <- factor(df_eda$HCM,
                     levels = c(1,2,8,9),
                     labels = c("Yes","No","Don't know","Prefer not to answer")
)

df_eda$hisp <- factor(df_eda$HISP,
                      levels = c(1,2,8,9),
                      labels = c("Yes","No","Don't know","Prefer not to answer")
)

df_eda$race_a <- factor(df_eda$RACE_A,
                        levels = c(1,2,3,4,5,6,8,9),
                        labels = c("White","Black or African American","Asian",
                                   "Native Hawaiian/Pacific Islander",
                                   "American Indian/Alaska Native","Other",
                                   "Don't know","Prefer not to answer")
)

df_eda$work <- factor(df_eda$WORK,
                      levels = c(1,2,3,4,5,6,7,8,9),
                      labels = c("Full-time","Part-time","Going to school","Other",
                                 "Unemployed looking for work","Disabled","Retired",
                                 "Don't know","Prefer not to answer")
)

df_eda$income <- factor(df_eda$INCOME,
                        levels = c(1,2,3,4,5,6,8,9),
                        labels = c("Under $25k","$25k-$35k","$35k-$50k",
                                   "$50k-$75k","$75k-$100k","Over $100k",
                                   "Don't know","Prefer not to answer")
)

df_eda$fintoday <- factor(df_eda$FINTODAY,
                          levels = c(1,2,3,8,9),
                          labels = c("Better off","Worse off","About the same",
                                     "Don't know","Prefer not to answer")
)

df_eda$fintoday <- factor(df_eda$fintoday,
                          levels = c("Worse off","About the same","Better off",
                                     "Don't know","Prefer not to answer")
)

df_eda$finfut <- factor(df_eda$FINFUT,
                        levels = c(1,2,3,8,9),
                        labels = c("Better off","Worse off","About the same",
                                   "Don't know","Prefer not to answer")
)

df_eda$finfut <- factor(df_eda$finfut,
                        levels = c("Worse off","About the same","Better off",
                                   "Don't know","Prefer not to answer")
)


df_eda$gender <- factor(df_eda$GENDER,
                        levels = c(1,2,3,5,9),
                        labels = c("Male","Female","Transgender",
                                   "Do not identify as female, male, or transgender",
                                   "Prefer not to answer")
)

df_eda$agerange <- factor(df_eda$AGERANGE,
                          levels = c(1,2,3,4,5,6),
                          labels = c("18-24","25-34","35-44","45-54","55-64","65+"))

### simple frequency tables

df_eda |> count(qol) |> mutate(prop = n / sum(n))
df_eda |> count(cc1) |> mutate(prop = n / sum(n))
df_eda |> count(cc4) |> mutate(prop = n / sum(n))
df_eda |> count(cc5) |> mutate(prop = n / sum(n))
df_eda |> count(cc8) |> mutate(prop = n / sum(n))
df_eda |> count(cc9) |> mutate(prop = n / sum(n))
df_eda |> count(income) |> mutate(prop = n / sum(n))
                          
                          


### practical EDA graphs


# overall quality of life

ggplot(df_eda, aes(x = qol, fill = qol)) +
  geom_bar() +
  labs(
    title = "Quality of Life in Grand Junction",
    x = "Quality of life rating",
    y = "Number of respondents"
  ) +
  theme_minimal()



# overall support for building CRC 

ggplot(df_eda, aes(x = cc1, fill = cc1)) +
  geom_bar() +
  labs(
    title = "Support for Building an Indoor Community Recreation Center",
    x = "Response",
    y = "Number of respondents"
  ) +
  theme_minimal()



# support by funding option 

ggplot(df_eda, aes(x = selfund, fill = cc4)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Funding Option",
    x = "Funding option shown to respondent",
    y = "Proportion",
    fill = "Support level"
  ) +
  theme_minimal()



# preffered CRC structure 

ggplot(df_eda, aes(x = cc5)) +
  geom_bar() +
  labs(
    title = "Preferred Type of Community Recreation Center",
    x = "Preferred option",
    y = "Number of respondents"
  ) +
  theme_minimal()


# location preference 

ggplot(df_eda, aes(x = cc6)) +
  geom_bar() +
  labs(
    title = "Preferred Location for a Large CRC",
    x = "Location preference",
    y = "Number of respondents"
  ) +
  theme_minimal()



# good use of city resources

ggplot(df_eda, aes(x = cc8)) +
  geom_bar() +
  labs(
    title = "CRC as a Good Use of City Resources",
    x = "Agreement level",
    y = "Number of respondents"
  ) +
  theme_minimal()



# expected use by admission price 

ggplot(df_eda, aes(x = selprice, fill = cc9)) +
  geom_bar(position = "fill") +
  labs(
    title = "Expected CRC Use by Daily Admission Price",
    x = "Randomly assigned daily admission price",
    y = "Proportion",
    fill = "Expected use"
  ) +
  theme_minimal()



# support by political party

ggplot(df_eda, aes(x = oparty, fill = cc1)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Political Party",
    x = "Political party",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal()



# support by age group 

ggplot(df_eda, aes(x = agerange, fill = cc1)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Age Group",
    x = "Age group",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal()


# support by income

ggplot(df_eda, aes(x = income, fill = cc1)) +
  geom_bar(position = "fill") +
  labs(
    title = "Support for CRC by Income",
    x = "Income",
    y = "Proportion",
    fill = "CRC support"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### actual interest fintoday and finfut

# base line understanding of percieved financial standing compared to last year 
ggplot(df_eda, aes(x = fintoday, fill = fintoday)) +
  geom_bar() +
  labs(title = "Perceived Financial Situation Compared to Last Year")

# base line understand of percived financial standing in the future 

ggplot(df_eda, aes(x = finfut, fill = finfut)) +
  geom_bar() +
  labs(title = "Expected Financial Situation Next Year")


# finanical perception and CRC support 

ggplot(df_eda, aes(x = fintoday, fill = cc1)) +
  geom_bar(position = "fill") +
  labs(title = "CRC Support by Financial Situation (Past)",
       y = "Proportion")

ggplot(df_eda, aes(x = finfut, fill = cc1)) +
  geom_bar(position = "fill") +
  labs(title = "CRC Support by Financial Outlook (Future)",
       y = "Proportion")

# financial perception and funding support

ggplot(df_eda, aes(x = fintoday, fill = cc4)) +
  geom_bar(position = "fill") +
  labs(title = "Funding Support by Financial Situation(Past)",
       y = "Proportion")

ggplot(df_eda, aes(x = finfut, fill = cc4)) +
  geom_bar(position = "fill") +
  labs(title = "Funding Support by Financial Situation(Future)",
       y = "Proportion")

# current financial perception compared to last year and actual income 

ggplot(df_eda, aes(x = income, fill = fintoday)) +
  geom_bar(position = "fill") +
  labs(title = "Income vs Perceived Financial Situation Compared To last year",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Future financial perception compared to today and actual income 
ggplot(df_eda, aes(x = income, fill = finfut)) +
  geom_bar(position = "fill") +
  labs(title = "Income vs Perceived Future Financial Situation ",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### summary stats

summary(df_eda)
prop.table(table(df_eda$cc1))
prop.table(table(df_eda$fintoday))
prop.table(table(df_eda$finfut))
