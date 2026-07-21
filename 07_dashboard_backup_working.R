library(shiny)
library(tidyverse)
library(DT)
library(scales)

model_data <- read.csv("data/processed/model_data.csv")
prospect_board <- read.csv("data/processed/prospect_board.csv")

player_master <- if (file.exists("data/raw/player_master.csv")) {
  read.csv("data/raw/player_master.csv")
} else {
  tibble(player_name = character(), full_name = character(),
         position = character(), bats_throws = character(),
         height = character(), weight = character())
}

name_lookup <- if (file.exists("data/raw/player_name_lookup.csv")) {
  read.csv("data/raw/player_name_lookup.csv") %>%
    distinct(player_name, .keep_all = TRUE)
} else {
  tibble(player_name = character(), full_name = character())
}
player_headshots <- if (file.exists("data/raw/player_headshots.csv")) {
  read.csv("data/raw/player_headshots.csv") %>%
    distinct(player_name, .keep_all = TRUE)
} else {
  tibble(player_name = character(), photo_url = character())
}
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

fmt3 <- function(x) sprintf("%.3f", as.numeric(x))
fmt_grade <- function(x) round(as.numeric(x), 1)
fmt_pct <- function(x) percent(as.numeric(x), accuracy = 0.1)

make_scouting_summary <- function(p) {
  power <- case_when(
    p$iso >= 0.300 ~ "plus game power",
    p$iso >= 0.200 ~ "above-average power",
    TRUE ~ "moderate power"
  )

  discipline <- case_when(
    p$bb_rate >= 0.12 ~ "strong plate discipline",
    p$bb_rate >= 0.08 ~ "solid strike-zone control",
    TRUE ~ "developing plate discipline"
  )

  contact <- case_when(
    p$k_rate <= 0.18 ~ "manageable swing-and-miss",
    p$k_rate <= 0.25 ~ "some swing-and-miss risk",
    TRUE ~ "notable swing-and-miss concerns"
  )

  paste0(
    p$display_name, " profiles with ", power, ", ",
    discipline, ", and ", contact, ". His offensive case is built around a ",
    fmt3(p$ops), " OPS, ", fmt3(p$iso),
    " ISO, and a ", fmt_grade(p$scout_grade),
    " scout grade on the 20-80 scale."
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

players <- players_all %>%
  group_by(player_name) %>%
  arrange(desc(signed_by_mlb_org), desc(season), desc(scout_grade)) %>%
  slice(1) %>%
  ungroup()

team_choices <- sort(unique(players$team))

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

  div(class = "hero",
      h1("⚾ Pioneer Scout"),
      h4("Independent League Scouting Platform")
  ),

  fluidRow(
    column(2,
      div(class = "sidebar",
        h3("Navigation"),
        actionLink("nav_targets", "🎯 Unsigned Targets", class = "nav-btn"),
        actionLink("nav_report", "👤 Player Report", class = "nav-btn"),
        actionLink("nav_analytics", "📊 Analytics", class = "nav-btn"),
        actionLink("nav_teams", "⚾ Teams", class = "nav-btn"),
        actionLink("nav_model", "🤖 Model", class = "nav-btn"),
        actionLink("nav_about", "ℹ About", class = "nav-btn")
      )
    ),
    column(6, uiOutput("main_page")),
    column(4, uiOutput("player_report"))
  )
)

server <- function(input, output, session) {

  page <- reactiveVal("targets")

  observeEvent(input$nav_targets, page("targets"))
  observeEvent(input$nav_report, page("report"))
  observeEvent(input$nav_analytics, page("analytics"))
  observeEvent(input$nav_teams, page("teams"))
  observeEvent(input$nav_model, page("model"))
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
    df <- filtered_players()
    row <- input$leaderboard_rows_selected

    if (length(row) == 0 || is.null(row)) {
      df[1, ]
    } else {
      df[row, ]
    }
  })

  output$main_page <- renderUI({

  if (page() == "targets") {

    tagList(
      fluidRow(
        column(4, div(class="card", div(class="metric", sum(players$signed_by_mlb_org == 0)), div(class="metric-label", "Unsigned Targets"))),
        column(4, div(class="card", div(class="metric", sum(players$signed_by_mlb_org == 1)), div(class="metric-label", "Historical MLB Signings"))),
        column(4, div(class="card", div(class="metric", max(fmt_grade(players$scout_grade), na.rm=TRUE)), div(class="metric-label", "Highest Scout Grade")))
      ),

      div(class="card",
        h2("🎯 Unsigned Targets"),

        fluidRow(
          column(6, sliderInput("min_pa", "Minimum Plate Appearances", min=min(players$pa, na.rm=TRUE), max=max(players$pa, na.rm=TRUE), value=250)),
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

        DTOutput("leaderboard")
      )
    )

  } else if (page() == "report") {

    div(class="card", h2("👤 Player Report"), p("Select a player from the scouting board. The report appears on the right."))

  } else if (page() == "analytics") {

    div(class="card", h2("📊 Analytics"), plotOutput("skill_chart", height="350px"))

  } else if (page() == "teams") {

    div(class="card", h2("⚾ Teams"), DTOutput("team_table"))

  } else if (page() == "model") {

    div(class="card",
      h2("🤖 Model"),
      p("Scout Grade is a 20-80 style score combining offensive production, power, discipline, and model output."),
      p("OPS measures overall offensive production. ISO means Isolated Power and is calculated as SLG minus AVG.")
    )

  } else {

    div(class="card",
      h2("ℹ About"),
      p("Pioneer Scout is an independent league scouting platform built in R/Shiny.")
    )
  }
})
  output$leaderboard <- renderDT({
    filtered_players() %>%
      transmute(
        Player = display_name,
        Team = team,
        Season = season,
        PA = pa,
        OPS = fmt3(ops),
        ISO = fmt3(iso),
        `Scout Grade` = fmt_grade(scout_grade),
        `Signing Probability` = fmt_pct(signing_probability),
        Status = signed_status
      ) %>%
      datatable(
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
  })

  output$player_report <- renderUI({
    p <- selected_player()

    if (nrow(p) == 0) return(div(class="card", "No player selected."))

    team_color <- team_colors[p$team]
    if (is.na(team_color)) team_color <- "#0b1f3a"

    stat_label_prefix <- ifelse(p$signed_by_mlb_org == 1, "Reference", "Most Recent")

    div(class="card",
     h3("👤 Scouting Report"),

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
      Skill = c("Power", "Discipline", "Overall"),
      Grade = c(
        scales::rescale(p$iso, to=c(20,80), from=c(0,0.45)),
        scales::rescale(p$bb_rate, to=c(20,80), from=c(0,0.2)),
        p$scout_grade
      )
    )

    ggplot(skill_df, aes(x=Skill, y=Grade)) +
      geom_col() +
      ylim(20, 80) +
      labs(x=NULL, y="20-80 Grade") +
      theme_minimal()
  })

  output$team_table <- renderDT({
    players %>%
      group_by(team) %>%
      summarise(
        Players = n(),
        `Avg Scout Grade` = round(mean(scout_grade, na.rm=TRUE), 1),
        `Top Scout Grade` = round(max(scout_grade, na.rm=TRUE), 1),
        `Signed Players` = sum(signed_by_mlb_org == 1),
        .groups = "drop"
      ) %>%
      arrange(desc(`Avg Scout Grade`)) %>%
      datatable(rownames=FALSE, options=list(pageLength=15))
  })
}

shinyApp(ui, server)