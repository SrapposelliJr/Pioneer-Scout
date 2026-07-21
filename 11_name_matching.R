library(tidyverse)
library(stringr)

make_name_key <- function(x) {
  x <- str_squish(x)

  first_part <- word(x, 1)
  last_part <- word(x, -1)

  first_initial <- str_sub(first_part, 1, 1)

  paste(
    str_to_lower(first_initial),
    str_to_lower(last_part),
    sep = "_"
  )
}

master <- read_csv("data/raw/player_master.csv", show_col_types = FALSE)
model <- read_csv("data/processed/model_data.csv", show_col_types = FALSE)

master_keys <- master %>%
  mutate(name_key = make_name_key(full_name)) %>%
  select(name_key, player_name_master = player_name, full_name, team, position, bats_throws, height, weight) %>%
  distinct(name_key, .keep_all = TRUE)

model_keys <- model %>%
  distinct(player_name) %>%
  mutate(name_key = make_name_key(player_name))

matches <- model_keys %>%
  left_join(master_keys, by = "name_key") %>%
  mutate(
    match_status = if_else(is.na(full_name), "needs_review", "matched")
  )

name_lookup <- matches %>%
  filter(match_status == "matched") %>%
  transmute(
    player_name,
    full_name
  ) %>%
  distinct()

needs_review <- matches %>%
  filter(match_status == "needs_review") %>%
  select(player_name, name_key)

write_csv(matches, "data/raw/player_name_matches.csv")
write_csv(name_lookup, "data/raw/player_name_lookup.csv")
write_csv(needs_review, "data/raw/player_name_needs_review.csv")

message("Total players: ", nrow(matches))
message("Auto-matched: ", nrow(name_lookup))
message("Needs review: ", nrow(needs_review))

head(matches, 30)