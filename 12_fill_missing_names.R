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

model <- read_csv("data/processed/model_data.csv", show_col_types = FALSE)
player_master <- read_csv("data/raw/player_master.csv", show_col_types = FALSE)

existing_lookup <- read_csv(
  "data/raw/player_name_lookup.csv",
  show_col_types = FALSE
)

top_targets <- model %>%
  filter(!is.na(scout_grade)) %>%
  group_by(player_name) %>%
  arrange(desc(scout_grade), desc(season)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(scout_grade)) %>%
  select(player_name, team, season, scout_grade, pa, ops, iso) %>%
  mutate(name_key = make_name_key(player_name))

master_keys <- player_master %>%
  mutate(name_key = make_name_key(full_name)) %>%
  select(
    name_key,
    full_name,
    master_team = team,
    position,
    bats_throws,
    height,
    weight
  ) %>%
  distinct(name_key, .keep_all = TRUE)

suggestions <- top_targets %>%
  left_join(master_keys, by = "name_key") %>%
  left_join(existing_lookup, by = "player_name", suffix = c("", "_existing")) %>%
  mutate(
    suggested_full_name = coalesce(full_name_existing, full_name),
    match_status = case_when(
      !is.na(full_name_existing) ~ "already_in_lookup",
      !is.na(full_name) ~ "auto_suggested",
      TRUE ~ "manual_needed"
    )
  ) %>%
  select(
    player_name,
    suggested_full_name,
    team,
    season,
    scout_grade,
    pa,
    ops,
    iso,
    master_team,
    position,
    bats_throws,
    height,
    weight,
    match_status
  )

auto_lookup <- suggestions %>%
  filter(match_status %in% c("already_in_lookup", "auto_suggested")) %>%
  transmute(
    player_name,
    full_name = suggested_full_name
  ) %>%
  distinct()

manual_review <- suggestions %>%
  filter(match_status == "manual_needed")

final_lookup <- bind_rows(
  existing_lookup,
  auto_lookup
) %>%
  filter(!is.na(player_name), !is.na(full_name)) %>%
  distinct(player_name, .keep_all = TRUE) %>%
  arrange(player_name)

write_csv(final_lookup, "data/raw/player_name_lookup.csv")
write_csv(suggestions, "data/raw/name_fill_suggestions.csv")
write_csv(manual_review, "data/raw/name_manual_review.csv")

message("Existing lookup rows: ", nrow(existing_lookup))
message("Final lookup rows: ", nrow(final_lookup))
message("Manual review needed: ", nrow(manual_review))

head(suggestions, 50)