library(tidyverse)
library(rvest)
library(janitor)
library(stringr)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

roster_urls <- tribble(
  ~team, ~url,
  "Oakland Ballers", "https://www.oaklandballers.com/sports/bsb/2026/roster",
  "Billings Mustangs", "https://www.billingsmustangs.com/sports/bsb/2026/roster",
  "Boise Hawks", "https://www.boisehawks.com/sports/bsb/2026/roster",
  "Glacier Range Riders", "https://www.gorangeriders.com/sports/bsb/2026/roster",
  "Great Falls Voyagers", "https://www.gfvoyagers.com/sports/bsb/2026/roster",
  "Idaho Falls Chukars", "https://www.ifchukars.com/sports/bsb/2026/roster",
  "Long Beach Coast", "https://longbeachcoast.com/sports/bsb/2026/roster",
  "Missoula PaddleHeads", "https://gopaddleheads.com/sports/bsb/2026/roster",
  "Modesto Roadsters", "https://modestoroadsters.com/sports/bsb/2026/roster",
  "Ogden Raptors", "https://ogden-raptors.com/sports/bsb/2026/roster",
  "Yuba-Sutter Freebirds", "https://freebirdsbaseball.com/sports/bsb/2026/roster"
)

clean_roster_field <- function(x) {
  x %>%
    str_replace_all("[\r\n\t]+", " ") %>%
    str_squish()
}

make_abbrev_name <- function(full_name) {
  full_name %>%
    str_replace("^([A-Za-z])[A-Za-z'\\.-]*\\s+", "\\1 ")
}

scrape_roster_safe <- function(team, url) {
  message("Scraping roster: ", team)

  tryCatch({
    page <- read_html(url)

    tables <- page %>%
      html_table(fill = TRUE)

    if (length(tables) == 0) {
      stop("No tables found")
    }

    roster <- tables[[1]] %>%
      clean_names() %>%
      mutate(
        source_team = team,
        source_url = url
      )

    message("  Success: ", team)
    roster

  }, error = function(e) {
    message("  Failed: ", team, " — ", e$message)

    tibble(
      source_team = team,
      source_url = url,
      scrape_error = e$message
    )
  })
}

player_master_raw <- roster_urls %>%
  mutate(data = map2(team, url, scrape_roster_safe)) %>%
  select(data) %>%
  unnest(data)

if ("scrape_error" %in% names(player_master_raw)) {
  failed_teams <- player_master_raw %>%
    filter(!is.na(scrape_error)) %>%
    distinct(source_team, source_url, scrape_error)

  valid_raw <- player_master_raw %>%
    filter(is.na(scrape_error)) %>%
    select(-scrape_error)
} else {
  failed_teams <- tibble(
    source_team = character(),
    source_url = character(),
    scrape_error = character()
  )

  valid_raw <- player_master_raw
}

if (nrow(failed_teams) > 0) {
  write_csv(failed_teams, "data/raw/player_master_failed_teams.csv")
}

player_master <- valid_raw %>%
  rename_with(~ str_replace_all(.x, "no_", "number")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(
    player_clean = if ("player" %in% names(.)) {
      clean_roster_field(player)
    } else {
      clean_roster_field(.[[1]])
    },

    full_name = player_clean %>%
      str_extract("^[A-Za-z'\\.-]+\\s+[A-Za-z'\\.-]+"),

    player_name = make_abbrev_name(full_name),

    position_clean = if ("position" %in% names(.)) clean_roster_field(position) else NA_character_,
    bats_throws_clean = if ("bats_throws" %in% names(.)) clean_roster_field(bats_throws) else NA_character_,
    height_clean = if ("height" %in% names(.)) clean_roster_field(height) else NA_character_,
    weight_clean = if ("weight" %in% names(.)) clean_roster_field(weight) else NA_character_,

    position_clean = str_replace(position_clean, "^Position:\\s*", ""),
    bats_throws_clean = str_replace(bats_throws_clean, "^Bats/Throws:\\s*", ""),
    height_clean = str_replace(height_clean, "^Height:\\s*", ""),
    weight_clean = str_replace(weight_clean, "^Weight:\\s*", ""),

    team = source_team
  ) %>%
  select(
    player_name,
    full_name,
    team,
    position = position_clean,
    bats_throws = bats_throws_clean,
    height = height_clean,
    weight = weight_clean,
    source_url
  ) %>%
  filter(
    !is.na(player_name),
    !is.na(full_name),
    player_name != "",
    full_name != ""
  ) %>%
  distinct(player_name, full_name, team, .keep_all = TRUE) %>%
  arrange(team, full_name)

write_csv(player_master, "data/raw/player_master.csv")

name_lookup <- player_master %>%
  select(player_name, full_name) %>%
  distinct() %>%
  arrange(player_name)

write_csv(name_lookup, "data/raw/player_name_lookup.csv")

message("Done.")
message("Players scraped: ", nrow(player_master))
message("Teams scraped successfully: ", n_distinct(player_master$team))

if (nrow(failed_teams) > 0) {
  message("Some teams failed. See data/raw/player_master_failed_teams.csv")
  print(failed_teams)
}

glimpse(player_master)
head(player_master, 20)