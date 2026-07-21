library(tidyverse)

model_data <- read_csv("data/processed/model_data.csv", show_col_types = FALSE)
player_master <- read_csv("data/raw/player_master.csv", show_col_types = FALSE)
headshots <- read_csv("data/raw/player_headshots.csv", show_col_types = FALSE)

missing_bios <- model_data %>%
  distinct(player_name, team, season) %>%
  left_join(
    player_master %>%
      select(player_name, full_name, position, bats_throws, height, weight),
    by = "player_name"
  ) %>%
  left_join(
    headshots %>%
      select(player_name, photo_url),
    by = "player_name"
  ) %>%
  mutate(
    missing_photo = is.na(photo_url) | photo_url == "",
    missing_height = is.na(height) | height == "",
    missing_weight = is.na(weight) | weight == "",
    missing_any = missing_photo | missing_height | missing_weight
  ) %>%
  filter(missing_any) %>%
  arrange(desc(season), team, player_name)

write_csv(missing_bios, "data/raw/missing_player_bios.csv")

message("Missing bios: ", nrow(missing_bios))
head(missing_bios, 50)