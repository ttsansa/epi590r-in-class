library(tidyverse)
library(broom)

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
    eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))
  )

# linear models
# create the model objects and store as mod_*
mod_sex_cat <- lm(income ~ sex_cat, data = nlsy)
mod_race_eth_cat <- lm(income ~ race_eth_cat, data = nlsy)
mod_eyesight_cat <- lm(income ~ eyesight_cat, data = nlsy)
mod_age_bir <- lm(income ~ age_bir, data = nlsy)

# tidy the models
# conf.int = TRUE gives confidence intervals
# store the results (coefficients and statistics) as tidy_*
tidy_sex_cat <- tidy(mod_sex_cat, conf.int = TRUE)
tidy_race_eth_cat <- tidy(mod_race_eth_cat, conf.int = TRUE)
tidy_eyesight_cat <- tidy(mod_eyesight_cat, conf.int = TRUE)
tidy_age_bir <- tidy(mod_age_bir, conf.int = TRUE)

# bind the results together and add a column for the model name
# bind_rows is like rbind, but it's more flexible
bind_rows(
  # assign names for each model coefficients
  sex_cat = tidy_sex_cat,
  race_eth_cat = tidy_race_eth_cat,
  eyesight_cat = tidy_eyesight_cat,
  age_bir = tidy_age_bir, .id = "model"
) |>
  mutate( # clean up the term names
    term = str_remove(term, model),
    term = ifelse(term == "", model, term)
  )

# fit a logistic model
logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
  data = nlsy, family = binomial()
)

# tidy the logistic model
# exponentiate = TRUE gives odds ratios
tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
  # tidy_categorical is a helper function to create a row for the reference category
  tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
  # remove some columns to make it easier to read
  select(-c(3:5))

# plot the logistic model
tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
  tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
  slice(-1) |> # remove intercept
  # the x axis is the level of the variable, the y axis is the estimate
  ggplot(mapping = aes(
    x = level, y = estimate,
    ymin = conf.low, ymax = conf.high
  )) +
  geom_point() +
  # add bars for the confidence intervals
  geom_errorbar() +
  # split the plot by variable
  facet_grid(cols = vars(variable), scales = "free", space = "free") +
  # make the y axis log scale since we are plotting odds ratios
  scale_y_log10()

#### Exercises ####

# 3
# Fit a log-binomial model and a Poisson model to the glasses data
eyes_binomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
  data = nlsy, family = binomial(link = "log")
)
eyes_poisson_model <- glm(glasses ~ eyesight_cat + sex_cat,
  data = nlsy, family = poisson(link = "log")
)

# usually I don't like adding packages in the middle of a script,
# but I'll do it here to be clear what they're used for
library(sandwich)
library(lmtest)

# tidy the models
# coeftest is used to get robust standard errors
binomial_tidy <- tidy(eyes_binomial_model, conf.int = TRUE)
poisson_tidy <- tidy(coeftest(eyes_poisson_model, vcov = vcovHC, type = "HC1"), conf.int = TRUE)

# create a dataframe of the estimates and confidence intervals from the two models
both_models <- bind_rows(
  binomial = binomial_tidy,
  poisson = poisson_tidy,
  .id = "model"
) |>
  # coeftest doesn't allow you to exponentiate, so do it here
  mutate(across(c(estimate, conf.low, conf.high), exp))

# compare estimates and CIs directly
both_models |>
  # remove some columns to make it easier to read
  select(-std.error, -statistic, -p.value) |>
  # "spread" the data so that each model is a column
  pivot_wider(
    values_from = c(estimate, conf.low, conf.high),
    names_from = model
  )
