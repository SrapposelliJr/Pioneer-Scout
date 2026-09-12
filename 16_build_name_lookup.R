library(tidyverse)
library(stringr)

# The Pioneer League stat feed abbreviates given names (for example, "S Canton").
# Baseball-Reference's league register supplies the matching full names and is
# keyed by season and player type, which avoids conflating players with the same
# initial and surname.
make_name_key <- function(x) {
  cleaned <- x %>%
    as.character() %>%
    str_replace_all("[’‘]", "'") %>%
    str_replace_all("[,#]", "") %>%
    str_replace_all("\\b(Jr\\.?|Sr\\.?|II|III|IV)\\b", "") %>%
    str_replace_all("[^A-Za-z' -]", " ") %>%
    str_squish()

  paste0(
    str_to_lower(str_sub(word(cleaned, 1), 1, 1)),
    "_",
    str_to_lower(word(cleaned, -1))
  )
}

clean_full_name <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("#", "") %>%
    str_squish()
}

model_targets <- read_csv("data/processed/model_data.csv", show_col_types = FALSE) %>%
  distinct(player_name, team, season) %>%
  transmute(player_name, team, season = as.integer(season), player_type = "hitter")

pitcher_targets <- read_csv("data/processed/pitcher_board.csv", show_col_types = FALSE) %>%
  distinct(name, team, season) %>%
  transmute(player_name = name, team, season = as.integer(season), player_type = "pitcher")

targets <- bind_rows(model_targets, pitcher_targets) %>%
  mutate(name_key = make_name_key(player_name)) %>%
  distinct()

age_names <- read_csv("data/raw/pioneer_player_ages.csv", show_col_types = FALSE) %>%
  transmute(
    season = as.integer(season),
    player_type,
    name_key = make_name_key(age_name),
    full_name = clean_full_name(age_name)
  ) %>%
  filter(!is.na(full_name), nzchar(full_name), name_key != "_") %>%
  distinct()

# Only accept an age-register match when its season/type/key maps to one full
# name.  Ambiguous abbreviations are handled by the manual roster fallback.
authoritative_names <- age_names %>%
  group_by(season, player_type, name_key) %>%
  filter(n_distinct(full_name) == 1) %>%
  ungroup() %>%
  distinct(season, player_type, name_key, .keep_all = TRUE)

old_lookup <- if (file.exists("data/raw/player_name_lookup.csv")) {
  read_csv("data/raw/player_name_lookup.csv", show_col_types = FALSE)
} else {
  tibble(player_name = character(), full_name = character())
}

manual_names <- bind_rows(
  old_lookup %>% select(any_of(c("player_name", "full_name"))),
  read_csv("data/raw/player_master.csv", show_col_types = FALSE) %>%
    select(player_name, full_name)
) %>%
  filter(!is.na(player_name), !is.na(full_name), nzchar(full_name)) %>%
  mutate(name_key = make_name_key(player_name), full_name = clean_full_name(full_name)) %>%
  select(name_key, full_name) %>%
  group_by(name_key) %>%
  filter(n_distinct(full_name) == 1) %>%
  ungroup() %>%
  distinct(name_key, .keep_all = TRUE)

current_roster_names <- read_csv("data/raw/player_headshots.csv", show_col_types = FALSE) %>%
  transmute(
    team,
    roster_key = make_name_key(player_name),
    roster_full_name = clean_full_name(full_name)
  ) %>%
  filter(!is.na(roster_full_name), nzchar(roster_full_name)) %>%
  group_by(team, roster_key) %>%
  filter(n_distinct(roster_full_name) == 1) %>%
  ungroup() %>%
  distinct(team, roster_key, .keep_all = TRUE)

lookup <- targets %>%
  left_join(authoritative_names, by = c("season", "player_type", "name_key")) %>%
  left_join(manual_names %>% rename(manual_full_name = full_name), by = "name_key") %>%
  left_join(current_roster_names, by = c("team", "name_key" = "roster_key")) %>%
  mutate(
    full_name = coalesce(full_name, if_else(season == 2026L, roster_full_name, NA_character_), manual_full_name),
    source = case_when(
      !is.na(full_name) & !is.na(roster_full_name) & full_name == roster_full_name ~ "Baseball-Reference register + current roster",
      !is.na(full_name) ~ "Baseball-Reference register",
      !is.na(manual_full_name) ~ "team roster archive",
      TRUE ~ "unresolved"
    )
  ) %>%
  rename(candidate_full_name = full_name) %>%
  group_by(player_name, season, player_type) %>%
  summarise(
    full_name = if (n_distinct(candidate_full_name, na.rm = TRUE) == 1) {
      first(candidate_full_name[!is.na(candidate_full_name)])
    } else {
      NA_character_
    },
    source = if (n_distinct(candidate_full_name, na.rm = TRUE) == 1) {
      first(source[!is.na(candidate_full_name)])
    } else {
      "unresolved"
    },
    .groups = "drop"
  ) %>%
  arrange(desc(season), player_type, player_name)

write_csv(lookup, "data/raw/player_name_lookup.csv")

coverage <- lookup %>%
  count(player_type, resolved = !is.na(full_name) & nzchar(full_name))
print(coverage)
message("Unresolved player-season records: ", sum(is.na(lookup$full_name) | !nzchar(lookup$full_name)))
