library(tidyverse)
library(rvest)
library(stringr)

current_season <- as.integer(format(Sys.Date(), "%Y"))

headshot_urls <- tribble(
  ~team, ~url,
  "Oakland Ballers", paste0("https://www.oaklandballers.com/sports/bsb/", current_season, "/roster?view=headshot"),
  "Billings Mustangs", paste0("https://www.billingsmustangs.com/sports/bsb/", current_season, "/roster?view=headshot"),
  "Boise Hawks", paste0("https://www.boisehawks.com/sports/bsb/", current_season, "/roster?view=headshot"),
  "Glacier Range Riders", paste0("https://www.gorangeriders.com/sports/bsb/", current_season, "/roster?view=headshot"),
  "Great Falls Voyagers", paste0("https://www.gfvoyagers.com/sports/bsb/", current_season, "/roster?view=headshot"),
  "Idaho Falls Chukars", paste0("https://www.ifchukars.com/sports/bsb/", current_season, "/roster?view=headshot"),
  "Long Beach Coast", paste0("https://longbeachcoast.com/sports/bsb/", current_season, "/roster?view=headshot"),
  "Missoula PaddleHeads", paste0("https://gopaddleheads.com/sports/bsb/", current_season, "/roster?view=headshot"),
  "Modesto Roadsters", paste0("https://modestoroadsters.com/sports/bsb/", current_season, "/roster?view=headshot"),
  "Ogden Raptors", paste0("https://ogden-raptors.com/sports/bsb/", current_season, "/roster?view=headshot"),
  "Yuba-Sutter Freebirds", paste0("https://freebirdsbaseball.com/sports/bsb/", current_season, "/roster?view=headshot")
)

make_abbrev_name <- function(full_name) {
  full_name %>%
    str_squish() %>%
    str_replace("^([A-Za-z])[A-Za-z'\\.-]*\\s+", "\\1 ")
}

fix_url <- function(src, page_url) {
  if (is.na(src) || src == "") return(NA_character_)
  if (str_starts(src, "http")) return(src)

  base <- str_extract(page_url, "^https?://[^/]+")
  paste0(base, src)
}

scrape_headshots <- function(team, url) {
  message("Scraping headshots: ", team)

  tryCatch({
    page <- read_html(url)

    cards <- page %>%
      html_elements(".sidearm-roster-player, .roster_player, li, .card")

    if (length(cards) == 0) {
      cards <- page %>% html_elements("img")
    }

    results <- map_dfr(cards, function(card) {
      img <- card %>% html_element("img")
      src <- img %>% html_attr("src")
      alt <- img %>% html_attr("alt")

      text <- card %>%
        html_text2() %>%
        str_squish()

      possible_name <- case_when(
        !is.na(alt) & str_count(alt, "\\w+") >= 2 ~ alt,
        str_count(text, "\\w+") >= 2 ~ text,
        TRUE ~ NA_character_
      )
possible_name <- possible_name %>%
  str_remove("\\s+bio photo$") %>%
  str_squish()
      tibble(
        team = team,
        full_name = possible_name,
        photo_url = fix_url(src, url)
      )
    })

    results %>%
      filter(
        !is.na(full_name),
        !is.na(photo_url),
        str_detect(photo_url, "jpg|jpeg|png|webp|ashx|image")
      ) %>%
      mutate(
        full_name = str_squish(full_name),
        player_name = make_abbrev_name(full_name)
      ) %>%
      select(player_name, full_name, team, photo_url) %>%
      distinct(player_name, full_name, team, .keep_all = TRUE)

  }, error = function(e) {
    message("  Failed: ", team, " — ", e$message)
    tibble(
      player_name = character(),
      full_name = character(),
      team = character(),
      photo_url = character()
    )
  })
}

previous_headshots <- if (file.exists("data/raw/player_headshots.csv")) {
  read_csv(
    "data/raw/player_headshots.csv",
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
} else {
  tibble(
    player_name = character(), full_name = character(),
    team = character(), photo_url = character()
  )
}

scraped_headshots <- headshot_urls %>%
  mutate(data = map2(team, url, scrape_headshots)) %>%
  select(data) %>%
  unnest(data)

healthy_teams <- scraped_headshots %>%
  count(team, name = "player_count") %>%
  filter(player_count >= 15) %>%
  pull(team)

if (length(healthy_teams) == 0) {
  message("No complete roster scrape was available; retaining the previous headshot data.")
  player_headshots <- previous_headshots
} else {
  # Replace a team only when its live roster is complete enough to trust. This
  # prevents a temporary source outage from clearing valid roster/headshot data.
  player_headshots <- bind_rows(
    scraped_headshots %>% filter(team %in% healthy_teams),
    previous_headshots %>% filter(!team %in% healthy_teams)
  )
}

player_headshots <- player_headshots %>%
  filter(!is.na(player_name), nzchar(player_name)) %>%
  distinct(player_name, .keep_all = TRUE)

write_csv(
  player_headshots,
  "data/raw/player_headshots.csv"
)

message("Done.")
message("Headshots found: ", nrow(player_headshots))

head(player_headshots, 30)
