#////////////////////////////////////////////////////////////////////////////////
## helper to get app key to then retrieve app secrets
get_app_key <- function() {
  require(rlang)

  key <- Sys.getenv("VULT_BEHAV_CLASSIF_APP_KEY")
  if (identical(key, "")) {
    rlang::abort(
      message = c(
        "No App key found",
        "x" = "You won't be able to proceed with the App testing. Sorry!",
        "i" = "You can request the App key from the App developers for testing purposes (bruno@dmpstats.co.uk)",
        "i" = "Set the provided App key via usethis::edit_r_environ() using enviroment variable named 'VULT_BEHAV_CLASSIF_APP_KEY'"
      )
    )
  }
  key
}


# /////////////////////////////////////////////////////////////////////////////
## helper to set interactive testing of main App RFunction (e.g. in testthat
## interactive mode, or on any given script)
##
set_interactive_app_testing <- function() {
  require(moveapps)
  source("RFunction.R")
  moveapps::logger.init()
  #source("src/common/logger.R")
  #source("src/io/app_files.R")

  options(dplyr.width = Inf)
}


## /////////////////////////////////////////////////////////////////////////////
# helper to run SDK testing with different settings
run_sdk <- function(
  data,
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
) {
  require(jsonlite)
  require(moveapps)
  require(jsonlite)

  # source App's functions
  source("RFunction.R")

  # get environmental variables specified in .env
  dotenv::load_dot_env(".env")

  # Housekeeping
  moveapps::logger.init()
  moveapps::clearRecentOutput()
  Sys.setenv(tz = "UTC")

  app_config_file <- Sys.getenv("CONFIGURATION")
  dflt_source_file <- moveapps::sourceFile()

  # Retain default app settings and input data
  dflt_app_config <- jsonlite::fromJSON(app_config_file) # moveapps::configuration()
  dflt_dt <- moveapps::readInput(dflt_source_file)

  # Ensure defaults are restored
  on.exit(
    {
      # reset config and data
      write(
        jsonlite::toJSON(dflt_app_config, pretty = TRUE, auto_unbox = TRUE),
        file = app_config_file
      )
      saveRDS(dflt_dt, dflt_source_file)
    },
    add = TRUE
  )

  # set specs to newly specified inputs
  new_app_config <- dflt_app_config
  new_app_config$travelcut <- travelcut
  new_app_config$create_plots <- create_plots
  new_app_config$sunrise_leeway <- sunrise_leeway
  new_app_config$sunset_leeway <- sunset_leeway
  new_app_config$altbound <- altbound
  new_app_config$keepAllCols <- keepAllCols

  # overwrite config file with current inputs
  write(
    jsonlite::toJSON(new_app_config, pretty = TRUE, auto_unbox = TRUE),
    file = app_config_file
  )

  # overwrite app's source file with current input data
  saveRDS(data, dflt_source_file)

  # Simulate running your app on MoveApps
  moveapps::runMoveAppsApp()

  invisible()
}
