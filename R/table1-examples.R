library(tidyverse)
library(gtsummary)

# Load and clean data
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


# simple table
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat, region_cat,
    eyesight_cat, glasses, age_bir
  )
)

# add labels for the variables and for the "missing" category
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat, region_cat,
    eyesight_cat, glasses, age_bir
  ),
  label = list(
    race_eth_cat ~ "Race/ethnicity",
    region_cat ~ "Region",
    eyesight_cat ~ "Eyesight",
    glasses ~ "Wears glasses",
    age_bir ~ "Age at first birth"
  ),
  missing_text = "Missing"
)

# add p-values, a total column, bold labels, and remove the footnote
tbl_summary(
  nlsy,
  by = sex_cat,
  include = c(
    sex_cat, race_eth_cat,
    eyesight_cat, glasses, age_bir
  ),
  label = list(
    race_eth_cat ~ "Race/ethnicity",
    eyesight_cat ~ "Eyesight",
    glasses ~ "Wears glasses",
    age_bir ~ "Age at first birth"
  ),
  missing_text = "Missing"
) |>
  # change the test used to compare sex_cat groups
  add_p(test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"
  )) |>
  # add a total column with the number of observations
  add_overall(col_label = "**Total** N = {N}") |>
  bold_labels() |>
  # remove the default footnotes
  modify_footnote(update = everything() ~ NA) |>
  # replace the column headers and make them bold
  modify_header(label = "**Variable**", p.value = "**P**")


#### Exercises ####

tbl_summary(nlsy,
  by = sex_cat,
  include = c(
    sex_cat, region_cat,
    race_eth_cat, income,
    # helper function to select columns that start with "sleep"
    starts_with("sleep")
  ),
  label = list(
    race_eth_cat ~ "Race/ethnicity",
    region_cat ~ "Region",
    income ~ "Income",
    sleep_wkdy ~ "Weekday sleep",
    sleep_wknd ~ "Weekday sleep"
  ),
  statistic = list(
    income ~ "{p10}, {p90}",
    starts_with("sleep") ~ "{min}, {max}"
  ),
  digits = list(
    income ~ c(3, 3), # or income ~ 3
    starts_with("sleep") ~ c(1, 1) # starts_with("sleep") ~ 1
  )
) |>
  # p-value and total column
  add_p() |>
  add_overall() |>
  # figured out how to do this from
  # https://stackoverflow.com/questions/73154658/adding-a-footnote-to-a-single-row-label-in-a-gtsummary-table
  modify_table_styling(
    columns = label,
    rows = label == "Race/ethnicity",
    footnote = "see https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data"
  )
