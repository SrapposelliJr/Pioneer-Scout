library(tidyverse)
library(rvest)
library(janitor)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

years <- 2021:2026

scrape_pbl_hitters <- function(year) {
  message("Scraping hitters: ", year)

  url <- paste0(
    "https://www.pioneerleague.com/sports/bsb/",
    year,
    "/players?sort=avg&view=&pos=h&r=0"
  )

  page <- httr::GET(
  url,
  httr::user_agent(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/126.0 Safari/537.36"
  )
) |>
  httr::content(as = "text", encoding = "UTF-8") |>
  xml2::read_html()

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

  offsets <- seq(0, 150, by = 25)
  collected <- list()

  for (offset in offsets) {
    message("  Offset: ", offset)

    url <- paste0(
      "https://www.pioneerleague.com/sports/bsb/",
      year,
      "/players?sort=ip&view=&pos=p&r=",
      offset
    )

    page <- tryCatch(
      {
        Sys.sleep(0.75)
        rvest::read_html(url)
      },
      error = function(e) {
        message("  Stopping at offset ", offset, ": ", e$message)
        return(NULL)
      }
    )

    # Stop pagination after the first failed request
    if (is.null(page)) {
      break
    }

    tables <- page %>%
      rvest::html_table(fill = TRUE)

    # Stop when the site returns no table
    if (length(tables) == 0) {
      message("  No table at offset ", offset, "; stopping.")
      break
    }

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
  stop(
    "Could not find pitcher table at offset ",
    offset,
    ". Tables found: ",
    length(tables)
  )
}

result <- tables[[pitcher_table_index]] %>%
  janitor::clean_names() %>%
  mutate(across(everything(), as.character)) %>%
  mutate(
    season = year,
    player_type = "pitcher"
  )

    # Stop if the returned table has no player records
    if (
      nrow(result) == 0 ||
      !"name" %in% names(result)
    ) {
      message("  No pitcher rows at offset ", offset, "; stopping.")
      break
    }

    collected[[length(collected) + 1]] <- result
  }

  if (length(collected) == 0) {
    warning("No pitcher data collected for ", year)
    return(tibble())
  }

  pitchers <- bind_rows(collected)

  required_keys <- c("season", "name", "team")

  if (all(required_keys %in% names(pitchers))) {
    pitchers <- pitchers %>%
      distinct(season, name, team, .keep_all = TRUE)
  }

  pitchers
}
 
scrape_pbl_alumni <- function() {
  message("Scraping alumni signings")

  url <- "https://www.pioneerleague.com/players/alumni-roster"
  page <- read_html(url)

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

pioneer_hitters_raw <- map_dfr(years, scrape_pbl_hitters)
pioneer_pitchers_raw <- scrape_pbl_pitchers(2026)
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