library(tidyverse)
library(scales)

hitters_player_level <- read_csv("data/processed/hitters_player_level.csv")
pitchers_clean <- read_csv("data/processed/pioneer_pitchers_clean.csv")
hitters_player_level <- hitters_player_level %>%
  select(
    -any_of(c(
      "lg_obp",
      "lg_slg",
      "ops_plus"
    ))
  )
league_hitting <- hitters_player_level %>%
  group_by(season) %>%
  summarise(
    lg_obp = sum(h + bb + hbp, na.rm = TRUE) /
      sum(ab + bb + hbp + sf, na.rm = TRUE),

    lg_slg = sum(
  h + x2b + 2 * x3b + 3 * hr,
  na.rm = TRUE
) /
  sum(ab, na.rm = TRUE),

    .groups = "drop"
  )

hitters_player_level <- hitters_player_level %>%
  left_join(league_hitting, by = "season") %>%
  mutate(
    ops_plus = round(
      100 * (
        obp / lg_obp +
        slg / lg_slg -
        1
      )
    )
  )
  write_csv(
  hitters_player_level,
  "data/processed/hitters_player_level.csv"
)
hitters_features <- hitters_player_level %>%
  filter(
    gp >= 1,
    pa > 0
  ) %>%
  mutate(
  ops_plus_pct = percent_rank(ops_plus),
  iso_pct = percent_rank(iso),
  hr_pct = percent_rank(hr_rate),
  bb_pct = percent_rank(bb_rate),
  k_pct = 1 - percent_rank(k_rate),

  power_score =
    0.60 * iso_pct +
    0.40 * hr_pct,

  discipline_score =
    0.55 * bb_pct +
    0.45 * k_pct,

  scout_score =
    0.40 * ops_plus_pct +
    0.25 * iso_pct +
    0.20 * bb_pct +
    0.15 * k_pct,

  # Small samples are pulled toward the league middle before grading. A
  # 45-grade is the Pioneer League baseline; 50 requires clearly
  # above-league production and each 10 points represents one SD.
  reliability = pa / (pa + 150),
  power_signal = (power_score - 0.5) * reliability,
  discipline_signal = (discipline_score - 0.5) * reliability,
  scout_signal = (scout_score - 0.5) * reliability
) %>%
  group_by(season) %>%
  mutate(
    power_z = (power_signal - mean(power_signal, na.rm = TRUE)) / sd(power_signal, na.rm = TRUE),
    discipline_z = (discipline_signal - mean(discipline_signal, na.rm = TRUE)) / sd(discipline_signal, na.rm = TRUE),
    scout_z = (scout_signal - mean(scout_signal, na.rm = TRUE)) / sd(scout_signal, na.rm = TRUE),
    power_grade = pmin(80, pmax(20, 5 * round((45 + 10 * power_z) / 5))),
    discipline_grade = pmin(80, pmax(20, 5 * round((45 + 10 * discipline_z) / 5))),
    scout_grade = pmin(80, pmax(20, 5 * round((45 + 10 * scout_z) / 5)))
) %>%
  ungroup() %>%
  arrange(desc(scout_grade))

scouting_board <- hitters_features %>%
  select(
    player_name,
    team,
    season,
    pa,
    ops,
    ops_plus,
    woba,
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
    app > 0
  ) %>%
  group_by(season) %>%
  mutate(
    gs_pct = if_else(app > 0, gs / app, 0),

    pitcher_role = if_else(
      gs_pct >= 0.50,
      "Starter",
      "Reliever"
    ),

    eligible = app > 0 & ip > 0,

    k_bb_pct = if_else(
      bf > 0,
      (k - bb) / bf,
      NA_real_
    )
  )

  pitcher_features <- pitcher_features %>%
  mutate(
    era_score = replace_na(percent_rank(-era), 0.5),
    fip_score = replace_na(percent_rank(-fip), 0.5),
    whip_score = replace_na(percent_rank(-whip), 0.5),
    command_score = replace_na(percent_rank(k_bb_pct), 0.5),
    workload_score = percent_rank(ip)
  )

  pitcher_features <- pitcher_features %>%
  mutate(
    pitcher_score_0_1 =
  0.15 * era_score +
  0.40 * fip_score +
  0.10 * whip_score +
  0.20 * command_score +
  0.15 * workload_score,

    # Pitcher samples are more volatile, so their baseline is set at 40 and
    # the spread is narrower than the hitter scale. A 50 still requires
    # clearly above-league work; 70+ is reserved for a truly rare outlier.
    reliability = ip / (ip + 30),
    pitcher_signal = (pitcher_score_0_1 - 0.5) * reliability
  ) %>%
  group_by(season) %>%
  mutate(
    pitcher_z = (pitcher_signal - mean(pitcher_signal, na.rm = TRUE)) / sd(pitcher_signal, na.rm = TRUE),
    pitcher_scout_grade = pmin(
      80,
      pmax(20, 5 * round((40 + 8 * pitcher_z) / 5))
    )
  ) %>%
  ungroup()

  write_csv(
  pitcher_features,
  "data/processed/pitcher_board.csv"
)
