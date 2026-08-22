library(shiny)
library(tidyverse)
library(DT)
library(scales)

model_data <- read.csv("data/processed/model_data.csv")
prospect_board <- read.csv("data/processed/prospect_board.csv")
pitchers_clean <- readr::read_csv(
  "data/processed/pioneer_pitchers_clean.csv",
  show_col_types = FALSE
)
pitcher_board <- readr::read_csv(
  "data/processed/pitcher_board.csv",
  show_col_types = FALSE
)
player_master <- if (file.exists("data/raw/player_master.csv")) {
  read.csv("data/raw/player_master.csv")
} else {
  tibble(player_name = character(), full_name = character(),
         position = character(), bats_throws = character(),
         height = character(), weight = character())
}
clean_bio_text <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all(
      c(
        "&rsquo;" = "'",
        "&#39;" = "'",
        "&rdquo;" = "\"",
        "&ldquo;" = "\"",
        "&quot;" = "\"",
        "&amp;" = "&"
      )
    ) %>%
    str_squish()
}

player_master <- player_master %>%
  mutate(
    height = clean_bio_text(height),
    weight = clean_bio_text(weight)
  )
name_lookup <- if (file.exists("data/raw/player_name_lookup.csv")) {
  read.csv("data/raw/player_name_lookup.csv") %>%
    distinct(player_name, .keep_all = TRUE)
} else {
  tibble(player_name = character(), full_name = character())
}
player_headshots <- if (file.exists("data/raw/player_headshots.csv")) {
  readr::read_csv(
    "data/raw/player_headshots.csv",
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) %>%
    distinct(player_name, .keep_all = TRUE)
} else {
  tibble(
    player_name = character(),
    full_name = character(),
    team = character(),
    photo_url = character()
  )
}

# A current team roster is authoritative for player availability.  A team is
# reconciled only when its roster scrape has a credible number of players, so a
# partial source response cannot accidentally clear an entire team.
make_roster_key <- function(x) {
  cleaned <- x %>%
    as.character() %>%
    str_replace_all("[^A-Za-z0-9 ]", " ") %>%
    str_squish()

  paste0(
    str_to_lower(str_sub(word(cleaned, 1), 1, 1)),
    "_",
    str_to_lower(word(cleaned, -1))
  )
}

roster_team_counts <- player_headshots %>%
  filter(!is.na(full_name), nzchar(full_name), !is.na(team), nzchar(team)) %>%
  count(team, name = "roster_count")

roster_covered_teams <- roster_team_counts %>%
  filter(roster_count >= 15) %>%
  pull(team)

current_roster <- player_headshots %>%
  filter(team %in% roster_covered_teams) %>%
  transmute(
    roster_key = make_roster_key(full_name),
    roster_team = team
  ) %>%
  filter(roster_key != "_") %>%
  distinct(roster_key, .keep_all = TRUE)

current_season <- max(c(model_data$season, pitcher_board$season), na.rm = TRUE)

reconcile_active_roster <- function(data, name_column) {
  data %>%
    mutate(roster_key = make_roster_key(.data[[name_column]])) %>%
    left_join(current_roster, by = "roster_key") %>%
    filter(
      season != current_season |
        !team %in% roster_covered_teams |
        !is.na(roster_team)
    ) %>%
    mutate(
      team = if_else(
        season == current_season & !is.na(roster_team),
        roster_team,
        team
      )
    ) %>%
    select(-roster_key, -roster_team)
}
last_updated <- if (
  file.exists("data/processed/last_updated.txt")
) {
  readLines(
    "data/processed/last_updated.txt",
    warn = FALSE
  )[1]
} else {
  "Not yet available"
}
pitcher_board <- pitcher_board %>%
  left_join(
    player_headshots %>%
      select(
        player_name,
        photo_url
      ),
    by = c("name" = "player_name")
  ) %>%
  left_join(
    name_lookup %>%
      select(
        player_name,
        full_name
      ),
    by = c("name" = "player_name")
  ) %>%
  mutate(
    display_name = if_else(
      !is.na(full_name) & nzchar(full_name),
      full_name,
      name
    )
  )

pitcher_board <- reconcile_active_roster(pitcher_board, "name")
pitchers_clean <- reconcile_active_roster(pitchers_clean, "name")
team_colors <- c(
  "Oakland Ballers" = "#00583D",
  "Billings Mustangs" = "#7B1FA2",
  "Boise Hawks" = "#E31B23",
  "Colorado Springs Sky Sox" = "#0057B8",
  "Glacier Range Riders" = "#6A1B9A",
  "Grand Junction Jackalopes" = "#8B4513",
  "Great Falls Voyagers" = "#005A9C",
  "Idaho Falls Chukars" = "#2E7D32",
  "Long Beach Coast" = "#00838F",
  "Missoula PaddleHeads" = "#FF7F27",
  "Modesto Roadsters" = "#444444",
  "Ogden Raptors" = "#005BBB",
  "Rocky Mountain Vibes" = "#FFC72C",
  "Yuba-Sutter Freebirds" = "#C62828"
)
team_logos <- c(
 "Oakland Ballers" = "https://www.oaklandballers.com/assets/Primary_Logo.png",
  "Billings Mustangs" = "https://www.billingsmustangs.com/assets/Primary_Logo.png",
  "Boise Hawks" = "https://www.boisehawks.com/assets/Primary_Logo.png",
  "Glacier Range Riders" = "https://www.gorangeriders.com/assets/Primary_Logo.png",
  "Great Falls Voyagers" = "https://www.gfvoyagers.com/assets/Primary_Logo.png",
  "Idaho Falls Chukars" = "https://www.ifchukars.com/assets/Primary_Logo.png",
  "Long Beach Coast" = "logos/long_beach_coast.png",
  "Missoula PaddleHeads" = "logos/missoula_paddleheads.png",
  "Modesto Roadsters" = "https://modestoroadsters.com/images/setup/Primary_Logo.png",
  "Ogden Raptors" = "https://ogden-raptors.com/assets/Primary_Logo.png",
  "Yuba-Sutter Freebirds" = "https://freebirdsbaseball.com/assets/Primary_Logo.png",
  "RedPocket Mobiles" = "logos/redpocket_mobiles.png"
)
mlb_logos <- c(
  "Arizona Diamondbacks" = "mlb_logos/arizona_diamondbacks.png",
  "Chicago Cubs" = "mlb_logos/chicago_cubs.png",
  "Chicago White Sox" = "mlb_logos/chicago_white_sox.png",
  "Cincinnati Reds" = "mlb_logos/cincinnati_reds.png",
  "Los Angeles Angels" = "mlb_logos/los_angeles_angels.png",
  "Milwaukee Brewers" = "mlb_logos/milwaukee_brewers.png",
  "Minnesota Twins" = "mlb_logos/minnesota_twins.png",
  "Pittsburgh Pirates" = "mlb_logos/pittsburgh_pirates.png"
)
fmt3 <- function(x) sprintf("%.3f", as.numeric(x))
fmt_grade <- function(x) round(as.numeric(x), 1)
fmt_pct <- function(x) percent(as.numeric(x), accuracy = 0.1)

make_scouting_summary <- function(p) {

  production <- case_when(
    p$ops_plus >= 150 ~ "elite league-adjusted production",
    p$ops_plus >= 125 ~ "well above-average league-adjusted production",
    p$ops_plus >= 110 ~ "above-average league-adjusted production",
    p$ops_plus >= 90  ~ "near-average league-adjusted production",
    TRUE ~ "below-average league-adjusted production"
  )

  power <- case_when(
    p$iso >= 0.300 ~ "plus game power",
    p$iso >= 0.200 ~ "above-average power",
    TRUE ~ "modest power production"
  )

  discipline <- case_when(
    p$bb_rate >= 0.12 ~ "strong plate discipline",
    p$bb_rate >= 0.08 ~ "solid strike-zone control",
    TRUE ~ "developing plate discipline"
  )

  contact <- case_when(
    p$k_rate <= 0.18 ~ "strong contact ability",
    p$k_rate <= 0.25 ~ "some swing-and-miss risk",
    TRUE ~ "notable swing-and-miss concerns"
  )

  signing_note <- case_when(
    p$signing_probability >= 0.20 ~
      "His statistical profile closely resembles past Pioneer League hitters who signed with MLB organizations.",

    p$signing_probability >= 0.10 ~
      "His statistical profile shows several similarities to past Pioneer League hitters who signed with MLB organizations.",

    p$signing_probability >= 0.05 ~
      "His statistical profile shows some traits associated with previous MLB signees.",

    TRUE ~
      "His current statistical profile is less similar to the historical Pioneer League players who later signed with MLB organizations."
  )

  paste0(
    p$display_name,
    " earns a ",
    fmt_grade(p$scout_grade),
    " scout grade due to ",
    production,
    " (OPS+ ",
    round(p$ops_plus),
    "), ",
    power,
    " (",
    fmt3(p$iso),
    " ISO), ",
    discipline,
    " (",
    fmt_pct(p$bb_rate),
    " BB%), and ",
    contact,
    " (",
    fmt_pct(p$k_rate),
    " K%). ",
    signing_note,
    " His estimated signing probability is ",
    fmt_pct(p$signing_probability),
    "."
  )
}
make_pitcher_summary <- function(p) {

  role <- ifelse(
    !is.na(p$gs_pct) && p$gs_pct >= 0.50,
    "starter",
    "reliever"
  )

  run_prevention <- case_when(
    p$fip <= 2.75 ~ "elite defense-independent run prevention",
    p$fip <= 3.50 ~ "above-average defense-independent run prevention",
    p$fip <= 4.25 ~ "solid defense-independent run prevention",
    p$fip <= 5.00 ~ "mixed defense-independent results",
    TRUE ~ "developing defense-independent run prevention"
  )

  command <- case_when(
    p$k_minus_bb_pct >= 0.25 ~ "excellent command",
    p$k_minus_bb_pct >= 0.18 ~ "strong command",
    p$k_minus_bb_pct >= 0.12 ~ "solid command",
    p$k_minus_bb_pct >= 0.06 ~ "average command",
    TRUE ~ "developing command"
  )

  bat_missing <- case_when(
    p$k_9 >= 12 ~ "plus swing-and-miss ability",
    p$k_9 >= 10 ~ "above-average bat-missing ability",
    p$k_9 >= 8 ~ "solid bat-missing ability",
    TRUE ~ "below-average strikeout production"
  )

  traffic <- case_when(
    p$whip <= 1.10 ~ "limits baserunners effectively",
    p$whip <= 1.30 ~ "keeps traffic manageable",
    p$whip <= 1.50 ~ "allows moderate traffic",
    TRUE ~ "has allowed frequent baserunners"
  )

  paste0(
    p$display_name,
    " profiles as a ",
    role,
    " with ",
    run_prevention,
    " (",
    formatC(p$fip, format = "f", digits = 2),
    " FIP), ",
    command,
    " (",
    scales::percent(p$k_minus_bb_pct, accuracy = 0.1),
    " K-BB%), and ",
    bat_missing,
    " (",
    formatC(p$k_9, format = "f", digits = 2),
    " K/9). He ",
    traffic,
    " with a ",
    formatC(p$whip, format = "f", digits = 2),
    " WHIP over ",
    formatC(p$ip, format = "f", digits = 1),
    " innings. This profile results in a ",
    formatC(p$pitcher_scout_grade, format = "f", digits = 1),
    " scout grade."
  )
}
players_all <- model_data %>%
  left_join(
    player_master %>%
      select(player_name, full_name_master = full_name,
             position, bats_throws, height, weight),
    by = "player_name"
  ) %>%
  left_join(
  name_lookup %>%
    select(
      player_name,
      full_name_lookup = full_name
    ),
  by = "player_name"
) %>%
left_join(
  player_headshots %>%
    select(
      player_name,
      photo_url
    ),
  by = "player_name"
) %>%
  left_join(
    prospect_board %>%
      select(player_name, season, signing_probability),
    by = c("player_name", "season")
  ) %>%
  mutate(
    display_name = coalesce(full_name_lookup, full_name_master, player_name),
    signing_probability = replace_na(signing_probability, 0),
    signed_status = ifelse(signed_by_mlb_org == 1, "Signed", "Unsigned")
  ) %>%
  filter(!is.na(scout_grade))

players_all <- reconcile_active_roster(players_all, "player_name")

 players <- players_all %>%
  mutate(season = as.integer(season)) %>%
  filter(
    season == 2026,
    gp >= 1,
    pa > 0
  ) %>%
  group_by(player_name, team, season) %>%
  arrange(desc(scout_grade)) %>%
  slice(1) %>%
  ungroup()
historical_signings <- players_all %>%
  mutate(season = as.integer(season)) %>%
  filter(signed_by_mlb_org == 1) %>%
  arrange(desc(season), desc(scout_grade))

inactive_teams <- c(
  "Colorado Springs Sky Sox",
  "Grand Junction Jackalopes",
  "Rocky Mountain Vibes"
)

team_choices <- sort(
  setdiff(
    unique(c(
      players$team,
      pitchers_clean$team
    )),
    inactive_teams
  )
)

hitter_stat_choices <- c(
  "Plate Appearances" = "PA",
  "OPS" = "OPS",
  "OPS+" = "OPS+",
  "ISO" = "ISO",
  "Signing Probability" = "Signing Probability"
)
hitter_stat_defaults <- unname(hitter_stat_choices)

pitcher_stat_choices <- c(
  "Innings Pitched" = "IP",
  "ERA" = "ERA",
  "FIP" = "FIP",
  "WHIP" = "WHIP",
  "K/9" = "K/9",
  "BB/9" = "BB/9",
  "HR/9" = "HR/9",
  "K/BB" = "K/BB"
)
pitcher_stat_defaults <- unname(pitcher_stat_choices)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background:#f4f6f8; font-family:Arial, sans-serif; color:#0b1f3a; }
      .hero { background:#0b1f3a; color:white; padding:28px; border-radius:0 0 10px 10px; margin-bottom:25px; }
      .sidebar { background:#101820; color:white; padding:20px; border-radius:10px; min-height:760px; }
      .nav-btn { display:block; color:white; background:#182b3d; padding:13px; margin-bottom:10px; border-radius:7px; font-weight:bold; text-decoration:none; }
      .nav-btn:hover { background:#24435f; color:white; text-decoration:none; }
      .card { background:white; padding:22px; border-radius:10px; box-shadow:0 2px 12px rgba(0,0,0,.08); margin-bottom:18px; }
      .metric { font-size:30px; font-weight:800; color:#0b1f3a; }
      .metric-label { color:#666; font-size:14px; }
      .player-name { font-size:30px; font-weight:800; color:#0b1f3a; }
      .team-pill { display:inline-block; color:white; padding:8px 14px; border-radius:18px; font-weight:bold; margin:8px 0; }
      .status-pill { display:inline-block; background:#f1f3f5; padding:7px 12px; border-radius:16px; font-weight:bold; margin-bottom:12px; }
      .stat-box { background:#f1f3f5; border-left:6px solid #0b1f3a; padding:16px; border-radius:7px; margin-bottom:12px; }
      .stat-value { font-size:25px; font-weight:800; color:#0b1f3a; }
      .stat-label { color:#666; font-size:14px; }
      .summary-box { border-top:1px solid #e5e7eb; margin-top:18px; padding-top:18px; line-height:1.6; }
    "))
  ),

 div(
    class = "hero",

    div(
  style = "
    display:flex;
    align-items:center;
    gap:18px;
    margin-bottom:8px;
  ",

  tags$img(
    src = "Pioneer Scout Logo.png",
    style = "
      width:70px;
      height:70px;
      object-fit:contain;
    "
  ),

  h1(
    "Pioneer Scout",
    style = "
      margin:0;
      color:white;
      font-size:56px;
      font-weight:800;
    "
  )
),

    h4("Independent League Scouting Platform"),

    p(
      paste("Database last updated:", last_updated),
      style = "
        margin-top:8px;
        margin-bottom:0;
        font-size:14px;
        color:#cbd5e1;
      "
    )
  ),

  fluidRow(
    column(
      2,
      div(
        class = "sidebar",

        h3("Navigation"),

        actionLink(
          "nav_targets",
          "⚾ Hitters",
          class = "nav-btn"
        ),

        actionLink(
          "nav_pitchers",
          "⚾ Pitchers",
          class = "nav-btn"
        ),

        actionLink(
          "nav_report",
          "👤 Player Report",
          class = "nav-btn"
        ),

        actionLink(
          "nav_analytics",
          "📊 Analytics",
          class = "nav-btn"
        ),

        actionLink(
          "nav_teams",
          "⚾ Teams",
          class = "nav-btn"
        ),

        actionLink(
          "nav_signings",
          "🏆 MLB Signings",
          class = "nav-btn"
        ),

        actionLink(
          "nav_about",
          "ℹ About",
          class = "nav-btn"
        )
      )
    ),

    column(
      6,
      uiOutput("main_page")
    ),

    column(
      4,
      conditionalPanel(
        condition = "output.showPlayerReport",
        uiOutput("player_report")
      )
    )
  )
)
server <- function(input, output, session) {

  page <- reactiveVal("targets")

output$showPlayerReport <- reactive({
  page() != "about"
})

outputOptions(output, "showPlayerReport", suspendWhenHidden = FALSE)

  selected_signing <- reactive({

    df <- historical_signings
    row <- input$signings_table_rows_selected

    if (is.null(row) || length(row) == 0) {

      df %>%
        arrange(desc(scout_grade)) %>%
        slice(1)

    } else {

      df[row[1], , drop = FALSE]

    }

  })

  observeEvent(input$nav_targets, page("targets"))
  observeEvent(input$nav_pitchers, page("pitchers"))
  observeEvent(input$nav_report, page("report"))
  observeEvent(input$nav_analytics, page("analytics"))
  observeEvent(input$nav_teams, page("teams"))
  observeEvent(input$nav_signings, page("signings"))
  observeEvent(input$nav_about, page("about"))

  filtered_players <- reactive({
    df <- players

    if (!is.null(input$min_pa)) {
      df <- df %>% filter(pa >= input$min_pa)
    }

    if (!is.null(input$team_filter) && input$team_filter != "All") {
      df <- df %>% filter(team == input$team_filter)
    }

    if (!is.null(input$status_filter)) {
      if (input$status_filter == "Unsigned") df <- df %>% filter(signed_by_mlb_org == 0)
      if (input$status_filter == "Signed") df <- df %>% filter(signed_by_mlb_org == 1)
    }
if (!is.null(input$player_search) &&
    input$player_search != "All") {

  df <- df %>%
    filter(display_name == input$player_search)

}
    df %>% arrange(desc(scout_grade))
  })

 selected_player <- reactive({
if (page() == "teams") {

  selected <- selected_team_roster_player()

  if (is.null(selected)) {
    return(players[0, ])
  }

  if (selected$player_type == "hitter") {
    return(
      players %>%
        filter(
          display_name == selected$display_name,
          team == selected$team,
          season == selected$season
        ) %>%
        slice(1)
    )
  }

  return(players[0, ])
}
  if (page() == "signings") {
    return(selected_signing())
  }

  if (page() == "teams" &&
      !is.null(input$team_roster_rows_selected)) {

    team_df <- players %>%
      filter(team == input$team_page_filter) %>%
      arrange(desc(scout_grade))

    row <- input$team_roster_rows_selected

    if (length(row) == 0) {
      team_df[1, ]
    } else {
      team_df[row, ]
    }

  } else {

    df <- filtered_players()
    row <- input$leaderboard_rows_selected

    if (length(row) == 0 || is.null(row)) {
      df[1, ]
    } else {
      df[row, ]
    }
  }
})
  output$main_page <- renderUI({

  if (page() == "targets") {

    tagList(
      fluidRow(
      column(4,
  div(class="card",
    div(
  class = "metric",
  length(
    union(
      unique(players$team),
      unique(pitcher_board$team)
    )
  )
),
    div(class="metric-label", "Current Teams")
  )
),

      div(class="card",
        h2("⚾ Hitters"),

        fluidRow(
          column(6, sliderInput("min_pa", "Minimum Plate Appearances", min=min(players$pa, na.rm=TRUE), max=max(players$pa, na.rm=TRUE), value=130)),
          column(6, selectInput("team_filter", "Team", choices=c("All", team_choices), selected="All"))
        ),

        selectizeInput(
          "player_search",
          "Search Player",
          choices = c("All", sort(unique(players$display_name))),
          selected = "All",
          options = list(placeholder = "Type a player name...")
        ),

        radioButtons(
          "status_filter",
          "Player Status",
          choices = c("Unsigned", "Signed", "All"),
          selected = "Unsigned",
          inline = TRUE
        ),

        checkboxGroupInput(
          "hitter_stats",
          "Stats to display",
          choices = hitter_stat_choices,
          selected = hitter_stat_defaults,
          inline = TRUE
        ),

        DTOutput("leaderboard")
      )
      )
    )

  } else if (page() == "report") {

    div(class="card", h2("👤 Player Report"), p("Select a player from the scouting board. The report appears on the right."))

} else if (page() == "pitchers") {

  tagList(
    div(
      class = "card",

      h2("⚾ Pioneer League Pitchers"),

      p("Current-season pitchers ranked by run prevention and command."),

      fluidRow(
        column(
          6,
        sliderInput(
        "min_pitcher_ip",
        "Minimum Innings Pitched",
         min = 0,
         max = 10,
         value = 0,
        step = 1
)
        ),

        column(
          6,
          selectInput(
            "pitcher_team",
            "Team",
            choices = c(
              "All",
              sort(unique(pitchers_clean$team))
            ),
            selected = "All"
          )
        )
      ),

      checkboxGroupInput(
        "pitcher_stats",
        "Stats to display",
        choices = pitcher_stat_choices,
        selected = pitcher_stat_defaults,
        inline = TRUE
      ),

      DT::DTOutput("pitchers_table")
    )
  )

  } else if (page() == "analytics") {

    div(class="card", h2("📊 Analytics"), plotOutput("skill_chart", height="350px"))

  } else if (page() == "teams") {

  tagList(
    div(class="card",
      h2("⚾ Team Overview"),
      selectInput(
        "team_page_filter",
        "Select Team",
        choices = team_choices,
        selected = team_choices[1]
      )
    ),

    uiOutput("team_profile")
  )
}else if (page() == "signings") {

  featured <- historical_signings %>%
    arrange(desc(scout_grade)) %>%
    slice(1)

  div(
  class = "card",

  h2("🏆 Historical MLB Signings"),

  p("Players from the dataset who later signed with MLB organizations."),

  fluidRow(
      column(4, div(class="stat-box",
        div(class="stat-value", nrow(historical_signings)),
        div(class="stat-label", "Signed Players")
      )),
      column(4, div(class="stat-box",
        div(class="stat-value", round(mean(historical_signings$scout_grade, na.rm=TRUE), 1)),
        div(class="stat-label", "Avg Scout Grade")
      )),
      column(4, div(class="stat-box",
        div(class="stat-value", max(historical_signings$season, na.rm=TRUE)),
        div(class="stat-label", "Most Recent Season")
      ))
    ),

       hr(),

    h3("⭐ Featured MLB Signing"),

    fluidRow(

      column(
  3,

  tags$div(

    style = "text-align:center;",

    tags$img(
      src = unname(mlb_logos[featured$organization]),
      style = "
        width:170px;
        height:170px;
        object-fit:contain;
        margin-bottom:15px;
      "
    ),

    h3(featured$organization),

    div(
      class = "team-pill",
      "MLB Organization"
    )

  )

),

      column(
        9,

        h2(featured$display_name),

        div(
          class="team-pill",
          paste(featured$team, "|", featured$season)
        ),

        br(), br(),

        fluidRow(

          column(
            4,
            div(class="stat-box",
                div(class="stat-value", fmt_grade(featured$scout_grade)),
                div(class="stat-label","Scout Grade"))
          ),

          column(
            4,
            div(class="stat-box",
                div(class="stat-value", fmt3(featured$ops)),
                div(class="stat-label","OPS"))
          ),

          column(
            4,
            div(class="stat-box",
                div(class="stat-value", fmt3(featured$iso)),
                div(class="stat-label","ISO"))
          )

        )

      )

    ),

    hr(),

    DTOutput("signings_table")


   )
  } else if (page() == "about") {

div(
  class = "card",
  style = "
    background:#ffffff;
    box-shadow:0 12px 30px rgba(15,23,42,.12);
  ",

  h2(
    "Meet the Creator",
    style = "
      margin-top:0;
      color:#08172d;
      font-weight:800;
    "
  ),

  div(
    style = "
      display:flex;
      align-items:flex-start;
      gap:32px;
      flex-wrap:wrap;
    ",

    tags$img(
      src = "scott_rapposelli.jpg",
      alt = "Scott Rapposelli Jr.",
      style = "
        width:220px;
        height:220px;
        object-fit:cover;
        object-position:center 20%;
        border-radius:50%;
        border:5px solid #e5e7eb;
        box-shadow:0 10px 24px rgba(15,23,42,.18);
      "
    ),

    div(
      style = "
        flex:1;
        min-width:280px;
      ",

      h3(
        "Scott Rapposelli Jr.",
        style = "
          margin-top:0;
          margin-bottom:16px;
          color:#08172d;
          font-size:30px;
          font-weight:800;
        "
      ),

      div(
        style = "
          display:grid;
          grid-template-columns:repeat(auto-fit,minmax(220px,1fr));
          gap:12px;
          margin-bottom:22px;
        ",

        div(
          style="
            background:#eef4fb;
            border-left:5px solid #0b1f3a;
            border:1px solid #d6e3f3;
            border-radius:10px;
            padding:14px 18px;
            color:#0b1f3a;
            font-weight:700;
          ",
          "🎓 Cal Poly San Luis Obispo"
        ),

        div(
          style="
            background:#eef4fb;
            border-left:5px solid #0b1f3a;
            border:1px solid #d6e3f3;
            border-radius:10px;
            padding:14px 18px;
            color:#0b1f3a;
            font-weight:700;
          ",
          "⚾ Player Development Analyst"
        ),

        div(
          style="
            background:#eef4fb;
            border-left:5px solid #0b1f3a;
            border:1px solid #d6e3f3;
            border-radius:10px;
            padding:14px 18px;
            color:#0b1f3a;
            font-weight:700;
          ",
          "📅 Graduating May 2027"
        ),

        div(
          style="
            background:#eef4fb;
            border-left:5px solid #0b1f3a;
            border:1px solid #d6e3f3;
            border-radius:10px;
            padding:14px 18px;
            color:#0b1f3a;
            font-weight:700;
          ",
          "⚾ Pursuing a Career in Major League Baseball"
        )
      ),
      hr(),

h3(
  "How Pioneer Scout Works",
  style = "
    color:#08172d;
    font-weight:800;
    margin-top:28px;
  "
),

p(
  "Pioneer Scout combines traditional baseball statistics with advanced metrics ",
  "to better evaluate player performance across the Pioneer League."
),

tags$ul(

  tags$li(
    tags$b("OPS+: "),
    "Adjusts a hitter's on-base percentage and slugging percentage relative to the Pioneer League average for that season. ",
    "A score of 100 is league average, while values above 100 indicate above-average offensive production."
  ),

  tags$li(
    tags$b("FIP: "),
    "Fielding Independent Pitching estimates a pitcher's performance using strikeouts, walks, hit batters, home runs, and innings pitched. ",
    "It removes much of the impact of team defense and is displayed on the same scale as ERA, where lower is better."
  ),
  tags$li(
  tags$b("Scout Grade: "),
  "A 20–80 offensive rating based on a player's performance relative to other Pioneer League hitters. ",
  "The current model weighs OPS+ (40%), ISO (25%), walk rate (20%), and strikeout rate (15%) to evaluate overall offensive ability."
),

tags$li(
  tags$b("Signing Probability: "),
  "An estimated probability generated from a logistic regression model trained on historical Pioneer League players who later signed with MLB organizations. ",
  "The model uses ISO, walk rate, strikeout rate, home run rate, and extra-base hit rate to estimate the likelihood of a future MLB signing."
)
),
tags$a(
  href = "https://github.com/Srapposellijr",
  target = "_blank",

  tags$button(
    "💻 View Pioneer Scout sourcecode on GitHub",

    style = "
      background:#0b1f3a;
      color:white;
      border:none;
      border-radius:8px;
      padding:12px 22px;
      font-size:16px;
      font-weight:700;
      cursor:pointer;
      margin-bottom:22px;
      margin-top:4px;
    "
  )
),

br(),
br(),
      p(
        "Created by Scott Rapposelli Jr., a student and Player Development Analyst at Cal Poly San Luis Obispo. Scott expects to graduate in May 2027 and is pursuing a career in Major League Baseball with interests in player development, scouting, baseball analytics, and baseball operations.",
        style="
          font-size:17px;
          line-height:1.8;
          color:#1f2937;
          font-weight:500;
          margin-bottom:0;
        "
      )
    )
  ),

  tags$hr(
    style="
      margin:32px 0;
      border:none;
      border-top:2px solid #d1d5db;
    "
  ),

  h2(
    "About Pioneer Scout",
    style="
      color:#08172d;
      font-weight:800;
      margin-bottom:12px;
    "
  ),

  p(
    "Pioneer Scout is an independent-league scouting and analytics platform designed to identify, evaluate, and track professional baseball talent. The platform combines automated data collection, feature engineering, statistical modeling, scouting reports, team analysis, and historical MLB signing information.",
    style="
      font-size:16px;
      line-height:1.8;
      color:#1f2937;
      font-weight:500;
    "
  ),

   p(
    "The project was built to surface overlooked players and provide a clearer view of performance, development indicators, and professional potential across the Pioneer League.",
    style = "
      font-size:16px;
      line-height:1.8;
      color:#1f2937;
      font-weight:500;
      margin-bottom:0;
    "
  )
)

}  # closes the About branch

}) # closes output$main_page <- renderUI({

  output$leaderboard <- renderDT({
    leaderboard_data <- filtered_players() %>%
      transmute(
        Rank = row_number(),
        Player = display_name,
        Team = team,
        Season = season,
        PA = pa,
        OPS = fmt3(ops),
        `OPS+` = round(ops_plus),
        ISO = fmt3(iso),
        `Scout Grade` = fmt_grade(scout_grade),
        `Signing Probability` = fmt_pct(signing_probability),
        Status = signed_status
      )

    selected_stats <- input$hitter_stats %||% hitter_stat_defaults
    displayed_columns <- c(
      "Rank", "Player", "Team", "Season", "Scout Grade",
      selected_stats, "Status"
    )

    leaderboard_data %>%
      select(all_of(unique(displayed_columns))) %>%
      datatable(
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
  })

  
   output$player_report <- renderUI({

  if (page() == "pitchers" || page() == "teams") {

  team_selection <- if (page() == "teams") {
    selected_team_roster_player()
  } else {
    NULL
  }

  show_pitcher <- page() == "pitchers" ||
    (!is.null(team_selection) &&
     team_selection$player_type == "pitcher")

  if (show_pitcher) {

    p <- if (page() == "teams") {

  pitchers_clean %>%
    filter(
      name == team_selection$display_name,
      team == team_selection$team,
      season == team_selection$season
    ) %>%
    slice(1)

} else {

  selected_pitcher()

}

    if (nrow(p) == 0) {
      return(
        div(
          class = "card",
          h3("⚾ Pitcher Scouting Report"),
          p("No pitchers meet the selected filters.")
        )
      )
    }

    pitcher_logo <- NULL

    if (
      !is.na(p$team) &&
      p$team %in% names(team_logos)
    ) {
      pitcher_logo <- team_logos[[p$team]]
    }

    safe_num <- function(x, digits = 2) {
      if (length(x) == 0 || is.na(x) || !is.finite(x)) {
        return("—")
      }

      formatC(x, format = "f", digits = digits)
    }

pitcher_photo <- if (
  "photo_url" %in% names(p) &&
  length(p$photo_url) > 0 &&
  !is.na(p$photo_url[[1]]) &&
  nzchar(p$photo_url[[1]])
) {
  as.character(p$photo_url[[1]])
} else {
  NULL
}

    pitcher_role <- ifelse(
      !is.na(p$gs_pct) && p$gs_pct >= 0.5,
      "Starter",
      "Reliever"
    )

    return(
      div(
        class = "card",

        h3("⚾ Pitcher Scouting Report"),

      if (!is.null(pitcher_photo)) {
  tags$img(
    src = pitcher_photo,
    style = "
      width:120px;
      height:120px;
      object-fit:cover;
      object-position:top center;
      border-radius:50%;
      margin-bottom:14px;
      border:3px solid #e5e7eb;
      background:#f8fafc;
    "
  )
},


       h2(
    p$display_name,
          style = "
            margin-bottom:8px;
            color:#07254a;
            font-weight:800;
          "
        ),

        div(
          paste0(p$team, " | ", p$season),
          style = "
            display:inline-block;
            background:#444;
            color:white;
            padding:9px 16px;
            border-radius:22px;
            font-weight:700;
            margin-bottom:10px;
          "
        ),

        br(),

        div(
          pitcher_role,
          style = "
            display:inline-block;
            background:#f1f3f5;
            color:#07254a;
            padding:8px 14px;
            border-radius:20px;
            font-weight:700;
            margin-bottom:14px;
          "
        ),

fluidRow(
  column(
    12,
    div(
      class = "metric",
    div(class = "metric-value", safe_num(p$pitcher_scout_grade, 1)),
      div(
        class = "metric-label",
        "Scout Grade"
      )
    )
  )
),

        fluidRow(
          column(
            6,
            div(
              class = "metric",
              div(class = "metric-value", safe_num(p$era)),
              div(class = "metric-label", "ERA")
            )
          ),
          column(
            6,
            div(
              class = "metric",
              div(class = "metric-value", safe_num(p$whip)),
              div(class = "metric-label", "WHIP")
            )
          )
        ),

        fluidRow(
          column(
            6,
            div(
              class = "metric",
              div(class = "metric-value", safe_num(p$k_9)),
              div(class = "metric-label", "K/9")
            )
          ),
          column(
            6,
            div(
              class = "metric",
              div(class = "metric-value", safe_num(p$bb_9)),
              div(class = "metric-label", "BB/9")
            )
          )
        ),

        fluidRow(
          column(
            6,
            div(
              class = "metric",
              div(class = "metric-value", safe_num(p$k_bb)),
              div(class = "metric-label", "K/BB")
            )
          ),
          column(
            6,
            div(
              class = "metric",
              div(class = "metric-value", safe_num(p$ip, 1)),
              div(class = "metric-label", "Innings Pitched")
            )
          )
        ),

        fluidRow(
          column(
            6,
            div(
              class = "metric",
              div(class = "metric-value", safe_num(p$hr_9)),
              div(class = "metric-label", "HR/9")
            )
          ),
          column(
            6,
            div(
              class = "metric",
              div(
                class = "metric-value",
                paste0(safe_num(p$k_minus_bb_pct * 100, 1), "%")
              ),
              div(class = "metric-label", "K-BB%")
            )
          )
        ),

         hr(),

        div(
          class = "summary-box",

          h4("📝 Why This Pitcher"),

          p(
            make_pitcher_summary(p)
          )
        )
      )
    )
  }
}
p <- selected_player()
    if (nrow(p) == 0) return(div(class="card", "No player selected."))

    team_color <- team_colors[p$team]
    if (is.na(team_color)) team_color <- "#0b1f3a"

    stat_label_prefix <- ifelse(p$signed_by_mlb_org == 1, "Reference", "Most Recent")
org_logo <- NULL

if (
  !is.null(p$organization) &&
  !is.na(p$organization) &&
  p$organization != "" &&
  p$organization %in% names(mlb_logos)
) {
  org_logo <- mlb_logos[[p$organization]]
}
    div(class="card",
     h3("👤 Scouting Report"),
if (!is.null(org_logo)) {
  tags$img(
    src = org_logo,
    style = "
      width:90px;
      height:90px;
      object-fit:contain;
      margin-bottom:12px;
    "
  )
},
if (!is.na(p$photo_url) && p$photo_url != "") {
  tags$img(
    src = p$photo_url,
    style = "
      width:120px;
      height:120px;
      border-radius:50%;
      object-fit:cover;
      margin-bottom:14px;
      border:4px solid #f1f3f5;
    "
  )
},

div(class = "player-name", p$display_name),
      div(class="team-pill", style=paste0("background:", team_color, ";"), paste(p$team, "|", p$season)),
      br(),
      div(class="status-pill", ifelse(p$signed_by_mlb_org == 1, "Signed Player / Historical Reference", "Unsigned / Current Target")),

      if (!is.na(p$position)) {
        tagList(
          br(),
          strong(paste(p$position, "|", p$bats_throws, "|", p$height, "|", p$weight)),
          br()
        )
      },

      br(),

      fluidRow(
        column(6, div(class="stat-box", div(class="stat-value", fmt_grade(p$scout_grade)), div(class="stat-label", paste(stat_label_prefix, "Scout Grade")))),
        column(6, div(class="stat-box", div(class="stat-value", fmt_pct(p$signing_probability)), div(class="stat-label", "Signing Probability")))
      ),

      fluidRow(
        column(6, div(class="stat-box", div(class="stat-value", fmt3(p$ops)), div(class="stat-label", paste(stat_label_prefix, "OPS")))),
        column(6, div(class="stat-box", div(class="stat-value", fmt3(p$iso)), div(class="stat-label", paste(stat_label_prefix, "ISO"))))
      ),

      fluidRow(
        column(6, div(class="stat-box", div(class="stat-value", fmt_pct(p$bb_rate)), div(class="stat-label", paste(stat_label_prefix, "BB%")))),
        column(6, div(class="stat-box", div(class="stat-value", fmt_pct(p$k_rate)), div(class="stat-label", paste(stat_label_prefix, "K%"))))
      ),

      fluidRow(
        column(6, div(class="stat-box", div(class="stat-value", fmt_pct(p$hr_rate)), div(class="stat-label", paste(stat_label_prefix, "HR%")))),
        column(6, div(class="stat-box", div(class="stat-value", p$pa), div(class="stat-label", paste(stat_label_prefix, "PA"))))
      ),

      div(class="summary-box",
        h4("📝 Why This Player"),
        p(make_scouting_summary(p))
      )
    )
  })

 output$skill_chart <- renderPlot({

  p <- selected_player()

  skill_df <- tibble(
    Skill = c(
      "Power",
      "Plate Discipline",
      "Contact",
      "Production",
      "Overall"
    ),
    Grade = c(
      scales::rescale(p$iso, to = c(20, 80), from = c(0, 0.45)),
      scales::rescale(p$bb_rate, to = c(20, 80), from = c(0, 0.20)),
      scales::rescale(1 - p$k_rate, to = c(20, 80), from = c(0.60, 1.00)),
      scales::rescale(p$ops, to = c(20, 80), from = c(0.600, 1.300)),
      p$scout_grade
    )
  )

  ggplot(skill_df,
         aes(
           x = reorder(Skill, Grade),
           y = Grade,
           fill = Grade
         )) +
    geom_col(width = 0.65) +
    coord_flip() +
    scale_y_continuous(
      limits = c(20, 80),
      breaks = seq(20, 80, 10)
    ) +
    scale_fill_gradient(
      low = "#d73027",
      high = "#1a9850",
      guide = "none"
    ) +
    geom_text(
      aes(label = round(Grade, 1)),
      hjust = -0.2,
      fontface = "bold",
      size = 5
    ) +
    labs(
      title = "20–80 Skill Profile",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
})


  
output$team_profile <- renderUI({

  req(input$team_page_filter)

 current_season <- max(
  c(players$season, pitchers_clean$season),
  na.rm = TRUE
)
print(input$team_page_filter)

print(unique(players$team))

print(unique(players$season))

print(current_season)
team_hitters <- players %>%
  filter(
    team == input$team_page_filter,
    season == current_season
  ) %>%
  arrange(desc(scout_grade))

team_pitchers <- pitchers_clean %>%
  filter(
    team == input$team_page_filter,
    season == current_season,
    app > 0
  ) %>%
  mutate(
    gs_pct = if_else(app > 0, gs / app, 0),
    pitcher_role = if_else(gs_pct >= 0.50, "Starter", "Reliever")
  ) %>%
  arrange(desc(ip))

top_hitter <- if (nrow(team_hitters) > 0) {
  team_hitters[1, , drop = FALSE]
} else {
  NULL
}

top_pitcher <- if (nrow(team_pitchers) > 0) {
  team_pitchers[1, , drop = FALSE]
} else {
  NULL
}

total_players <- nrow(team_hitters) + nrow(team_pitchers)

  div(class = "card",
    tags$div(
      style = "text-align:center; margin-bottom:20px;",
      tags$img(
        src = team_logos[input$team_page_filter],
        style = "
          width:120px;
          max-height:120px;
          object-fit:contain;
          margin-bottom:15px;
        "
      ),
      h2(input$team_page_filter)
    ),

    fluidRow(
  column(
    4,
    div(
      class = "stat-box",
      div(class = "stat-value", nrow(team_hitters)),
      div(class = "stat-label", "Hitters")
    )
  ),

  column(
    4,
    div(
      class = "stat-box",
      div(class = "stat-value", nrow(team_pitchers)),
      div(class = "stat-label", "Pitchers")
    )
  ),

  column(
    4,
    div(
      class = "stat-box",
      div(class = "stat-value", total_players),
      div(class = "stat-label", "Total Players")
    )
  )
),

    hr(),

   h3("⭐ Top Hitter"),

if (is.null(top_hitter)) {
  div(
    class = "stat-box",
    div(class = "stat-label", "No current hitters for this team")
  )
} else {
  fluidRow(
    column(
      4,
      if (
        !is.na(top_hitter$photo_url) &&
        top_hitter$photo_url != ""
      ) {
        tags$img(
          src = top_hitter$photo_url,
          style = "
            width:110px;
            height:110px;
            border-radius:50%;
            object-fit:cover;
            border:4px solid #f1f3f5;
          "
        )
      }
    ),

    column(
      8,
      h3(top_hitter$display_name),
      strong(
        paste(
          top_hitter$team,
          "|",
          top_hitter$season
        )
      ),
      br(),
      br(),

      div(
        class = "stat-box",
        div(
          class = "stat-value",
          fmt_grade(top_hitter$scout_grade)
        ),
        div(class = "stat-label", "Scout Grade")
      ),

      fluidRow(
        column(
          6,
          div(
            class = "stat-box",
            div(class = "stat-value", fmt3(top_hitter$ops)),
            div(class = "stat-label", "OPS")
          )
        ),

        column(
          6,
          div(
            class = "stat-box",
            div(class = "stat-value", fmt3(top_hitter$iso)),
            div(class = "stat-label", "ISO")
          )
        )
      )
    )
  )
},

hr(),

h3("⚾ Top Pitcher"),

if (is.null(top_pitcher)) {
  div(
    class = "stat-box",
    div(class = "stat-label", "No current pitchers for this team")
  )
} else {
  fluidRow(
    column(
      4,
      tags$img(
        src = team_logos[[input$team_page_filter]],
        style = "
          width:110px;
          height:110px;
          object-fit:contain;
        "
      )
    ),

    column(
      8,
      h3(top_pitcher$name),
      strong(
        paste(
          top_pitcher$team,
          "|",
          top_pitcher$season
        )
      ),
      br(),
      br(),

      div(
        style = "
          display:inline-block;
          background:#f1f3f5;
          padding:7px 13px;
          border-radius:18px;
          font-weight:700;
          margin-bottom:12px;
        ",
        top_pitcher$pitcher_role
      ),

      fluidRow(
        column(
          4,
          div(
            class = "stat-box",
            div(
              class = "stat-value",
              round(top_pitcher$ip, 1)
            ),
            div(class = "stat-label", "IP")
          )
        ),

        column(
          4,
          div(
            class = "stat-box",
            div(
              class = "stat-value",
              round(top_pitcher$era, 2)
            ),
            div(class = "stat-label", "ERA")
          )
        ),

        column(
          4,
          div(
            class = "stat-box",
            div(
              class = "stat-value",
              round(top_pitcher$whip, 2)
            ),
            div(class = "stat-label", "WHIP")
          )
        )
      )
    )
  )
},

    hr(),

    h3("Roster"),

    DTOutput("team_roster")
  )
})
output$team_roster <- renderDT({

  req(input$team_page_filter)

  current_season <- max(
   c(players$season, pitcher_board$season),
    na.rm = TRUE
  )

  hitter_roster <- players %>%
    filter(
      team == input$team_page_filter,
      season == current_season
    ) %>%
    transmute(
      Player = display_name,
      Type = "Hitter",
      Role = dplyr::if_else(
  !is.na(position) & position != "",
  as.character(position),
  "Position Player",
  missing = "Position Player"
),
      `Scout Grade` = as.character(fmt_grade(scout_grade)),
      `Primary Stat` = ifelse(
  is.na(ops),
  "N/A",
  paste0(sprintf("%.3f", ops), " OPS")
),
      Status = signed_status
    )

  pitcher_roster <- pitcher_board %>%
  # existing pitcher code
  transmute(
    Player = display_name,
    Type = "Pitcher",
    Role = pitcher_role,
    `Scout Grade` = as.character(fmt_grade(pitcher_scout_grade)),
    `Primary Stat` = paste0(
      formatC(era, format = "f", digits = 2),
      " ERA"
    ),
    Status = "Current"
  )

hitter_roster <- hitter_roster %>%
  mutate(
    Role = as.character(Role),
    `Primary Stat` = as.character(`Primary Stat`)
  )

pitcher_roster <- pitcher_roster %>%
  mutate(
    Role = as.character(Role),
    `Primary Stat` = as.character(`Primary Stat`)
  )

combined_roster <- bind_rows(
  hitter_roster,
  pitcher_roster
) %>%
  arrange(Type, Player)

  datatable(
    combined_roster,
    rownames = FALSE,
    selection = "single",
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      autoWidth = TRUE
    )
  )
})

selected_team_roster_player <- reactive({

  req(input$team_page_filter)

  current_season <- max(
    c(players$season, pitchers_clean$season),
    na.rm = TRUE
  )

  hitter_roster <- players %>%
    filter(
      team == input$team_page_filter,
      season == current_season
    ) %>%
    transmute(
      display_name = display_name,
      player_type = "hitter",
      team = team,
      season = season,
      source_row = row_number()
    )

  pitcher_roster <- pitchers_clean %>%
    filter(
      team == input$team_page_filter,
      season == current_season,
      app > 0
    ) %>%
    transmute(
      display_name = name,
      player_type = "pitcher",
      team = team,
      season = season,
      source_row = row_number()
    )

  combined_lookup <- bind_rows(
    hitter_roster,
    pitcher_roster
  ) %>%
    arrange(player_type, display_name)

  row <- input$team_roster_rows_selected

  if (
    is.null(row) ||
    length(row) == 0 ||
    row > nrow(combined_lookup)
  ) {
    return(NULL)
  }

  combined_lookup[row, , drop = FALSE]
})

  filtered_pitchers <- reactive({

  latest_season <- max(pitcher_board$season, na.rm = TRUE)

 df <- pitcher_board %>%
  filter(
    season == latest_season,
    app > 0,
    ip > 0
  ) %>%
  mutate(
    pitcher_role = if_else(
      gs_pct >= 0.50,
      "Starter",
      "Reliever"
    )
  )

  if (!is.null(input$min_pitcher_ip)) {
    df <- df %>% filter(ip >= input$min_pitcher_ip)
  }

  if (
    !is.null(input$pitcher_team) &&
    input$pitcher_team != "All"
  ) {
    df <- df %>%
      filter(team == input$pitcher_team)
  }

  df %>%
    arrange(
      desc(pitcher_scout_grade),
      era,
      whip
    )
})


output$pitchers_table <- renderDT({

  pitcher_table_data <- filtered_pitchers() %>%
  transmute(
    Rank = row_number(),
    Pitcher = name,
    Team = team,
    Role = pitcher_role,
    `Scout Grade` = round(pitcher_scout_grade, 1),
    IP = round(ip, 1),
    ERA = round(era, 2),
    FIP = round(fip, 2),
    WHIP = round(whip, 2),
    `K/9` = round(k_9, 2),
    `BB/9` = round(bb_9, 2),
    `HR/9` = round(hr_9, 2),
    `K/BB` = round(k_bb, 2)
  )

  selected_stats <- input$pitcher_stats %||% pitcher_stat_defaults
  displayed_columns <- c(
    "Rank", "Pitcher", "Team", "Role", "Scout Grade", selected_stats
  )

pitcher_table_data %>%
  select(all_of(unique(displayed_columns))) %>%
  datatable(
  rownames = FALSE,
  selection = "single",
  options = list(
    pageLength = 15,
    scrollX = TRUE
  )
)
})


selected_pitcher <- reactive({

  df <- filtered_pitchers()

  if (nrow(df) == 0) {
    return(df)
  }

  row <- input$pitchers_table_rows_selected

  if (is.null(row) || length(row) == 0 || row > nrow(df)) {
    row <- 1
  }

  df[row, , drop = FALSE]
})

output$signings_table <- renderDT({

  historical_signings %>%
    transmute(
      Player = display_name,
      Team = team,
      Season = season,
      PA = pa,
      OPS = fmt3(ops),
      ISO = fmt3(iso),
      `Scout Grade` = as.character(fmt_grade(scout_grade)),
      `Signing Probability` = fmt_pct(signing_probability),
      Status = signed_status
    ) %>%
    datatable(
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
