library(tidyverse)
library(gtsummary)

# load and clean data
nlsy_cols <- c(
  "glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
  "id", "nsibs", "samp", "race_eth", "sex", "region",
  "income", "res_1980", "res_2002", "age_bir"
)
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
  na = c("-1", "-2", "-3", "-4", "-5", "-998"),
  skip = 1, col_names = nlsy_cols
) |>
  mutate(
    region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
    sex_cat = factor(sex, labels = c("Male", "Female")),
    race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
    eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
    glasses_cat = factor(glasses, labels = c("No", "Yes"))
  )


# Univariate regression
# this funciton is BOTH running the regressions and creating the table

# regression of income on a series of predictor (x) variables
tbl_uvregression(
  nlsy,
  y = income,
  include = c(
    sex_cat, race_eth_cat,
    eyesight_cat, income, age_bir
  ),
  method = lm
)
## this is equivalent to
# lm(income ~ sex_cat, data = nlsy)
# lm(income ~ race_eth_cat, data = nlsy)
# lm(income ~ eyesight_cat, data = nlsy)
# lm(income ~ income, data = nlsy)
# lm(income ~ age_bir, data = nlsy)


# regression of glasses on a series of predictor (x) variables
# using a logistic regression model
tbl_uvregression(
  nlsy,
  y = glasses,
  include = c(
    sex_cat, race_eth_cat,
    eyesight_cat, glasses, age_bir
  ),
  method = glm,
  method.args = list(family = binomial()),
  exponentiate = TRUE
)


## Multivariable regressions
# now we need to fit the models first

# linear model
linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
  data = nlsy
)

# linear model with interaction
linear_model_int <- lm(income ~ sex_cat * age_bir + race_eth_cat,
  data = nlsy
)

# logistic model
logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
  data = nlsy, family = binomial()
)


## Tables
# we use the models we just fit to create the tables
tbl_regression(
  linear_model,
  # include the intercept
  intercept = TRUE,
  # relabel the variables
  label = list(
    sex_cat ~ "Sex",
    race_eth_cat ~ "Race/ethnicity",
    age_bir ~ "Age at first birth"
  )
)


tbl_regression(
  logistic_model,
  exponentiate = TRUE,
  label = list(
    sex_cat ~ "Sex",
    eyesight_cat ~ "Eyesight"
  )
)


# in order to compare the models with and without interaction
# we need to create and store the tables first

# table for the model without interaction
tbl_no_int <- tbl_regression(
  linear_model,
  intercept = TRUE,
  label = list(
    sex_cat ~ "Sex",
    race_eth_cat ~ "Race/ethnicity",
    age_bir ~ "Age at first birth"
  )
)

# table for the model with interaction
tbl_int <- tbl_regression(
  linear_model_int,
  intercept = TRUE,
  label = list(
    sex_cat ~ "Sex",
    race_eth_cat ~ "Race/ethnicity",
    age_bir ~ "Age at first birth",
    `sex_cat:age_bir` ~ "Sex/age interaction"
  )
)

## Table comparing the models with and without interaction
tbl_merge(list(tbl_no_int, tbl_int),
  tab_spanner = c("**Model 1**", "**Model 2**")
)


