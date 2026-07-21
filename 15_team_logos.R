library(tidyverse)
library(rvest)
library(stringr)

dir.create("www/logos", recursive = TRUE, showWarnings = FALSE)

team_sites <- tribble(
  ~team, ~site,
  "Oakland Ballers", "https://www.oaklandballers.com",
  "Billings Mustangs", "https://www.billingsmustangs.com",
  "Boise Hawks", "https://www.boisehawks.com",
  "Glacier Range Riders", "https://www.gorangeriders.com",
  "Great Falls Voyagers", "https://www.gfvoyagers.com",
  "Idaho Falls Chukars", "https://www.ifchukars.com",
  "Long Beach Coast", "https://longbeachcoast.com",
  "Missoula PaddleHeads", "https://gopaddleheads.com",
  "Modesto Roadsters", "https://modestoroadsters.com",
  "Ogden Raptors", "https://ogden-raptors.com",
  "Yuba-Sutter Freebirds", "https://freebirdsbaseball.com"
)

safe_name <- function(team) {
  team %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

fix_url <- function(src, site) {
  if (is.na(src) || src == "") return(NA_character_)
  if (str_starts(src, "http")) return(src)
  if (str_starts(src, "//")) return(paste0("https:", src))
  paste0(site, src)
}

scrape_logo <- function(team, site) {
  message("Finding logo: ", team)

  page <- read_html(site)

  imgs <- page %>%
    html_elements("img")

  candidates <- tibble(
    src = imgs %>% html_attr("src"),
    alt = imgs %>% html_attr("alt"),
    class = imgs %>% html_attr("class")
  ) %>%
    mutate(
      src = map_chr(src, fix_url, site = site),
      score = 0,
      score = score + if_else(str_detect(str_to_lower(coalesce(alt, "")), "logo"), 5, 0),
      score = score + if_else(str_detect(str_to_lower(coalesce(class, "")), "logo"), 5, 0),
      score = score + if_else(str_detect(str_to_lower(coalesce(src, "")), "logo"), 4, 0),
      score = score + if_else(str_detect(str_to_lower(coalesce(src, "")), "png|svg|webp|jpg|jpeg"), 2, 0)
    ) %>%
    filter(!is.na(src), score > 0) %>%
    arrange(desc(score))

  if (nrow(candidates) == 0) {
    message("  No logo found: ", team)
    return(tibble(team = team, logo_url = NA_character_, file = NA_character_))
  }

  logo_url <- candidates$src[1]
  ext <- str_extract(logo_url, "\\.(png|svg|webp|jpg|jpeg)") %>%
    str_remove("\\.")

  if (is.na(ext)) ext <- "png"

  file <- file.path("www/logos", paste0(safe_name(team), ".", ext))

  tryCatch({
    download.file(logo_url, file, mode = "wb", quiet = TRUE)
    tibble(team = team, logo_url = logo_url, file = file)
  }, error = function(e) {
    message("  Failed download: ", team)
    tibble(team = team, logo_url = logo_url, file = NA_character_)
  })
}

logo_results <- team_sites %>%
  mutate(data = map2(team, site, scrape_logo)) %>%
  select(data) %>%
  unnest(data)

write_csv(logo_results, "data/raw/team_logo_results.csv")

message("Done.")
print(logo_results)
list.files("www/logos")