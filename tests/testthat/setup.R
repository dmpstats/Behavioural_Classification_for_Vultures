Sys.setenv("USER_APP_FILE_HOME_DIR" = "../../data/auxiliary/user-files")
remotes::install_github("movestore/moveapps-sdk-r-package")
moveapps::logger.init()

moveapps::clearRecentOutput()
# the system under test (sut)
source(file.path("..", "..", "./RFunction.R"))


source("../app-testing-helpers.r")
app_key <- get_app_key()

`%!in%` <- Negate(`%in%`)
