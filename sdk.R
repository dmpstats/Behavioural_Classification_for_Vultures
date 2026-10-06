# You can control your local app development via environment variables.
# You can define things like input-data, app-configuration etc.
# Per default your environment is defined in `/.env`
dotenv::load_dot_env()

# This loads and installs the MoveApps R SDK
remotes::install_github("movestore/moveapps-sdk-r-package")
moveapps::logger.init()
moveapps::clearRecentOutput()

library("moveapps")
Sys.setenv(tz = "UTC")
# `./RFunction.R` is the home of your app code
# It is the only file which will be bundled into the final app on MoveApps
source("RFunction.R")

# Lets simulate running your app on MoveApps
moveapps::runMoveAppsApp()
