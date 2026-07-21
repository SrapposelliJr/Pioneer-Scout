library(tidyverse)
library(scales)

hitters_player_level <- read_csv("data/processed/hitters_player_level.csv")
pitchers_clean <- read_csv("data/processed/pioneer_pitchers_clean.csv")
hitters_features <- hitters_player_level %>%
  filter(pa >= 250) %>%
  mutate(
    iso_pct = percent_rank(iso),
    hr_pct = percent_rank(hr_rate),
    bb_pct = percent_rank(bb_rate),
    k_pct = 1 - percent_rank(k_rate),

    power_score = 0.60 * iso_pct + 0.40 * hr_pct,
    discipline_score = 0.55 * bb_pct + 0.45 * k_pct,

    scout_score = 0.65 * power_score + 0.35 * discipline_score,

    power_grade = rescale(power_score, to = c(20, 80)),
    discipline_grade = rescale(discipline_score, to = c(20, 80)),
    scout_grade = rescale(scout_score, to = c(20, 80))
  ) %>%
  arrange(desc(scout_grade))

scouting_board <- hitters_features %>%
  select(
    player_name,
    team,
    season,
    pa,
    ops,
    iso,
    hr_rate,
    bb_rate,
    k_rate,
    power_grade,
    discipline_grade,
    scout_grade
  )

write.csv(
  scouting_board,
  "data/processed/scouting_board.csv",
  row.names = FALSE
)

head(scouting_board, 25)

pitcher_features <- pitchers_clean %>%
  filter(
    season == max(season, na.rm = TRUE),
    app > 0
  ) %>%
  mutate(
    gs_pct = if_else(app > 0, gs / app, 0),

    pitcher_role = if_else(
      gs_pct >= 0.50,
      "Starter",
      "Reliever"
    ),

    minimum_ip = if_else(
      pitcher_role == "Starter",
      30,
      15
    ),

    eligible = ip >= minimum_ip,

    k_bb_pct = if_else(
      bf > 0,
      (k - bb) / bf,
      NA_real_
    )
  )

  pitcher_features <- pitcher_features %>%
  mutate(
    era_score = percent_rank(-era),
    whip_score = percent_rank(-whip),
    k9_score = percent_rank(k_9),
    command_score = percent_rank(k_bb_pct),
    hr9_score = percent_rank(-hr_9),
    workload_score = percent_rank(ip)
  )

  pitcher_features <- pitcher_features %>%
  mutate(
    pitcher_score_0_1 =
      0.25 * era_score +
      0.20 * whip_score +
      0.20 * k9_score +
      0.20 * command_score +
      0.10 * hr9_score +
      0.05 * workload_score,

    pitcher_scout_grade = pmin(
      80,
      pmax(
        20,
        20 + 60 * pitcher_score_0_1
      )
    )
  )

  write_csv(
  pitcher_features,
  "data/processed/pitcher_board.csv"
)
