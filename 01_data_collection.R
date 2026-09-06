library(tidyverse)
library(rvest)
library(janitor)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

years <- 2021:2026

fetch_pioneer_page <- function(url, attempts = 2L) {
  for (attempt in seq_len(attempts)) {
    response <- httr::GET(
      url,
      httr::user_agent(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/126.0 Safari/537.36"
      )
    )

    body <- httr::content(response, as = "text", encoding = "UTF-8")
    rate_limited <-
      httr::status_code(response) == 429 ||
      stringr::str_detect(body, stringr::regex("heavy bot traffic|temporarily rate limited", ignore_case = TRUE))

    if (httr::status_code(response) < 400 && !rate_limited) {
      return(xml2::read_html(body))
    }

    if (attempt < attempts) {
      wait_seconds <- if (rate_limited) 70 else 15
      message("  Pioneer League request was unavailable; retrying in ", wait_seconds, " seconds.")
      Sys.sleep(wait_seconds)
    }
  }

  stop("Pioneer League request failed after ", attempts, " attempts: ", url)
}

read_previous_data <- function(path, label) {
  if (!file.exists(path)) {
    stop("No previously collected ", label, " data is available for fallback.")
  }

  warning("Using the last successful ", label, " data because the live scrape was unavailable.")
  readr::read_csv(path, show_col_types = FALSE)
}

scrape_pbl_hitters <- function(year) {
  message("Scraping hitters: ", year)

  url <- paste0(
    "https://www.pioneerleague.com/sports/bsb/",
    year,
    "/players?sort=avg&view=&pos=h&r=0"
  )

  page <- fetch_pioneer_page(url)

  hitters <- page %>%
    html_table(fill = TRUE) %>%
    pluck(1) %>%
    clean_names() %>%
    mutate(across(everything(), as.character)) %>%
    mutate(
      season = year,
      player_type = "hitter"
    )

  hitters
}
scrape_pbl_pitchers <- function(year) {
  message("Scraping pitchers: ", year)

  url <- paste0(
    "https://www.pioneerleague.com/sports/bsb/",
    year,
    "/players?sort=ip&view=&pos=p&r=0"
  )

  tables <- fetch_pioneer_page(url) %>%
    rvest::html_table(fill = TRUE)

  pitcher_table_index <- which(
    vapply(
      tables,
      function(tbl) {
        cleaned_names <- janitor::make_clean_names(names(tbl))
        all(c("era", "ip", "whip") %in% cleaned_names)
      },
      logical(1)
    )
  )[1]

  if (is.na(pitcher_table_index)) {
    stop("Could not find a pitcher table for ", year, ".")
  }

  tables[[pitcher_table_index]] %>%
    janitor::clean_names() %>%
    mutate(across(everything(), as.character)) %>%
    mutate(
      season = as.character(year),
      player_type = "pitcher"
    ) %>%
    distinct(season, name, team, .keep_all = TRUE)
}
 
scrape_pbl_alumni <- function() {
  message("Scraping alumni signings")

  url <- "https://www.pioneerleague.com/players/alumni-roster"
  page <- fetch_pioneer_page(url)

  alumni <- page %>%
    html_table(fill = TRUE) %>%
    pluck(1) %>%
    clean_names() %>%
    mutate(across(everything(), as.character)) %>%
    mutate(
      name = name %>%
        str_replace_all("[\r\n\t]+", " ") %>%
        str_squish() %>%
        str_replace("^([A-Za-z'. -]+) \\1$", "\\1"),

      organization = organization %>%
        str_replace_all("[\r\n\t]+", " ") %>%
        str_replace("Organization:", "") %>%
        str_squish(),

      team = team %>%
        str_replace_all("[\r\n\t]+", " ") %>%
        str_replace("Team:", "") %>%
        str_squish(),

      position = position %>%
        str_replace_all("[\r\n\t]+", " ") %>%
        str_replace("Position:", "") %>%
        str_squish(),

      hometown = hometown %>%
        str_replace_all("[\r\n\t]+", " ") %>%
        str_replace("Hometown:", "") %>%
        str_squish()
    )

  alumni
}

pioneer_hitters_raw <- tryCatch(
  map_dfr(years, scrape_pbl_hitters),
  error = function(e) {
    message("Hitters scrape failed: ", conditionMessage(e))
    read_previous_data("data/raw/pioneer_hitters_raw.csv", "hitters")
  }
)

pitcher_archive <- if (file.exists("data/raw/pioneer_pitchers_raw.csv")) {
  readr::read_csv("data/raw/pioneer_pitchers_raw.csv", show_col_types = FALSE) %>%
    mutate(across(everything(), as.character))
} else {
  tibble::tibble()
}

current_pitchers <- tryCatch(
  scrape_pbl_pitchers(2026),
  error = function(e) {
    message("Pitchers scrape failed: ", conditionMessage(e))
    tibble::tibble()
  }
)

pioneer_pitchers_raw <- if (nrow(current_pitchers) > 0) {
  bind_rows(
    pitcher_archive %>% filter(season != 2026),
    current_pitchers
  )
} else {
  pitcher_archive
}

if (nrow(pioneer_pitchers_raw) == 0) {
  pioneer_pitchers_raw <- read_previous_data("data/raw/pioneer_pitchers_raw.csv", "pitchers")
}
pioneer_alumni_raw <- tryCatch(
  scrape_pbl_alumni(),
  error = function(e) {
    warning("Alumni scrape failed: ", conditionMessage(e))
    tibble::tibble()
  }
)

write.csv(
  pioneer_hitters_raw,
  "data/raw/pioneer_hitters_raw.csv",
  row.names = FALSE
)

write.csv(
  pioneer_pitchers_raw,
  "data/raw/pioneer_pitchers_raw.csv",
  row.names = FALSE
)

if (nrow(pioneer_alumni_raw) > 0) {
  write.csv(
    pioneer_alumni_raw,
    "data/raw/pioneer_alumni_raw.csv",
    row.names = FALSE
  )
}

list.files("data/raw")
