library(tidyverse)

# ---------------------------------------
# Load Model Data
# ---------------------------------------

model_data <- read.csv("data/processed/model_data.csv")

# Keep complete cases for the model
model_df <- model_data %>%
  filter(
    !is.na(iso),
    !is.na(bb_rate),
    !is.na(k_rate),
    !is.na(hr_rate),
    !is.na(xbh_rate)
  )

# ---------------------------------------
# Train Logistic Regression
# ---------------------------------------

signing_model <- glm(
  signed_by_mlb_org ~
    iso +
    bb_rate +
    k_rate +
    hr_rate +
    xbh_rate,
  data = model_df,
  family = "binomial"
)

summary(signing_model)

# ---------------------------------------
# Predict Probability
# ---------------------------------------

model_df <- model_df %>%
  mutate(
    signing_probability =
      predict(
        signing_model,
        newdata = model_df,
        type = "response"
      )
  )

# ---------------------------------------
# Top Prospects
# ---------------------------------------

prospect_board <- model_df %>%
  arrange(desc(signing_probability)) %>%
  select(
    player_name,
    team,
    season,
    pa,
    ops,
    iso,
    scout_grade,
    signing_probability,
    signed_by_mlb_org
  )

head(prospect_board, 25)

# ---------------------------------------
# Save
# ---------------------------------------

write.csv(
  prospect_board,
  "data/processed/prospect_board.csv",
  row.names = FALSE
)