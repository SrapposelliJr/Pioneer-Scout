library(tidyverse)
library(janitor)
library(stringr)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

hitters_raw <- read_csv("data/raw/pioneer_hitters_raw.csv")

clean_numeric <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all(",", "") %>%
    na_if("-") %>%
    as.numeric()
}

clean_ip <- function(x) {
  x <- clean_numeric(x)

  whole_innings <- floor(x)
  outs <- round((x - whole_innings) * 10)

  whole_innings + outs / 3
}

hitters_clean <- hitters_raw %>%
  clean_names() %>%
  mutate(
    name = str_replace_all(name, "[\r\n\t]", " "),
    name = str_squish(name),
    team = str_squish(team),
    season = as.integer(season),

    across(
      c(gp, ab, h, rbi, bb, x2b, x3b, hr, xbh, k,
        avg, obp, slg, hbp, sf, sh, hdp, go, fo, go_fo, pa),
      clean_numeric
    ),

    player_name = name,
    ops = obp + slg,
    iso = slg - avg,
    bb_rate = bb / pa,
    k_rate = k / pa,
    hr_rate = hr / pa,
    xbh_rate = xbh / pa
  )

hitters_player_level <- hitters_clean %>%
  filter(pa >= 100) %>%
  group_by(player_name) %>%
  slice_max(
    order_by = pa,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()
pitchers_raw <- read_csv(
  "data/raw/pioneer_pitchers_raw.csv",
  show_col_types = FALSE
)

pitchers_clean <- pitchers_raw %>%
  clean_names() %>%
  mutate(
    name = str_replace_all(name, "[\r\n\t]", " "),
    name = str_squish(name),
    team = str_squish(team),
    season = as.integer(season),

    across(
      any_of(c(
        "rk", "era", "w", "l", "app", "gs", "sv",
        "h", "r", "er", "bb", "k", "k_9", "hr",
        "whip", "bf", "wp", "hbp"
      )),
      clean_numeric
    ),

    ip_raw = clean_numeric(ip),
    ip = clean_ip(ip),
    player_type = "pitcher"
  ) %>%
  filter(
    !is.na(name),
    name != "",
    !is.na(team),
    team != ""
  ) %>%
  mutate(
    bb_9 = if_else(ip > 0, bb * 9 / ip, NA_real_),
    hr_9 = if_else(ip > 0, hr * 9 / ip, NA_real_),
    h_9 = if_else(ip > 0, h * 9 / ip, NA_real_),
    k_bb = if_else(bb > 0, k / bb, NA_real_),
    k_pct = if_else(bf > 0, k / bf, NA_real_),
    bb_pct = if_else(bf > 0, bb / bf, NA_real_),
    k_minus_bb_pct = k_pct - bb_pct,
    gs_pct = if_else(app > 0, gs / app, NA_real_)
  )
write_csv(
  hitters_clean,
  "data/processed/hitters_clean.csv"
)

write_csv(
  pitchers_clean,
  "data/processed/pioneer_pitchers_clean.csv"
)

write_csv(
  hitters_player_level,
  "data/processed/hitters_player_level.csv"
)

glimpse(hitters_clean)

hitters_player_level %>%
  select(player_name, team, season, pa, ops, iso, bb_rate, k_rate, hr_rate) %>%
  arrange(desc(ops)) %>%
  head(25)
  list.files("data/processed")
