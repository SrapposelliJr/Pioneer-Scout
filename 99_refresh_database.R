# 99_refresh_database.R
# Refreshes all data files used by Pioneer Scout.

options(warn = 1)

project_dir <- getwd()

log_file <- file.path(project_dir, "daily_refresh_log.txt")


log_message <- function(...) {
  message_text <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ",
    paste0(..., collapse = "")
  )

  message(message_text)

  cat(
    message_text,
    "\n",
    file = log_file,
    append = TRUE
  )
}

run_step <- function(script_name) {
  log_message("Starting ", script_name)

  if (!file.exists(script_name)) {
    stop("Missing script: ", script_name)
  }

  source(
    script_name,
    local = new.env(parent = globalenv()),
    echo = FALSE
  )

  log_message("Completed ", script_name)
}

log_message("Pioneer Scout refresh started")

tryCatch(
  {
    run_step("01_data_collection.R")
    run_step("02_data_cleaning.R")
    run_step("03_feature_engineering.R")
    run_step("04_model_training.R")
    run_step("05_model_validation.R")
    run_step("13_player_headshots.R")
writeLines(
  format(
    Sys.time(),
    "%B %d, %Y at %I:%M %p %Z",
    tz = "America/Los_Angeles"
  ),
  "data/processed/last_updated.txt"
)
    log_message("Pioneer Scout refresh completed successfully")
  },
  error = function(e) {
    log_message("REFRESH FAILED: ", conditionMessage(e))
    stop(e)
  }
)
