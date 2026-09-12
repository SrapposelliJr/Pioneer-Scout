library(tidyverse)
library(scales)
library(stringr)

# Load broader player-level data
hitters <- read.csv("data/processed/hitters_player_level.csv")
scouting_board <- read.csv("data/processed/scouting_board.csv")

alumni <- read.csv("data/raw/pioneer_alumni_raw.csv")

make_name_key <- function(x) {
  x <- str_squish(x)
  x <- str_replace_all(x, "\\b(Jr\\.|Jr|Sr\\.|Sr|II|III|IV)\\b", "")
  x <- str_squish(x)

  first_initial <- str_sub(x, 1, 1)
  last_name <- word(x, -1)

  str_to_lower(paste0(first_initial, last_name))
}

hitters_model <- hitters %>%
  filter(
    gp >= 1,
    pa > 0
  ) %>%
  mutate(
    name_key = make_name_key(player_name)
  ) %>%
  left_join(
    scouting_board %>%
      select(
        player_name,
        team,
        season,
        power_grade,
        discipline_grade,
        scout_grade
      ),
    by = c("player_name", "team", "season")
  )

alumni_clean <- alumni %>%
  mutate(
    alumni_name = name,
    name_key = make_name_key(alumni_name)
  )

model_data <- hitters_model %>%
  left_join(
    alumni_clean %>%
      select(name_key, alumni_name, organization),
    by = "name_key"
  ) %>%
  mutate(
    signed_by_mlb_org = if_else(is.na(alumni_name), 0, 1)
  )

write.csv(
  model_data,
  "data/processed/model_data.csv",
  row.names = FALSE
)

model_data %>%
  count(signed_by_mlb_org)

model_data %>%
  filter(signed_by_mlb_org == 1) %>%
  select(player_name, alumni_name, organization, season, pa, ops, scout_grade) %>%
  arrange(desc(scout_grade))
