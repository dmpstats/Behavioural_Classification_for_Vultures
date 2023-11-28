# ------------------------- #
#         Preamble
# ------------------------- #

library(move2)
library(httr2)
library(purrr)

# Helpers
source("tests/app-testing-helpers.r")

# get App secret key for decrypting test dataset
app_key <- get_app_key()

# Read (encrypted) input datasets for testing
test_dt <- secret_read_rds("data/raw/vult_test_data.rds", key = I(app_key))

test_dt$metadata

map(test_dt, ~all(is.na(.x$altitude)))


# ---------------------------------------- #
# ----   Interactive RFunction testing  ----
# ---------------------------------------- #

set_interactive_app_testing()

rFunction(
  data = test_dt$savahn, # test_dt$nam_sop, 
  rooststart = 18, 
  roostend = 7, 
  travelcut = 3,
  create_plots = FALSE,
  use_sunrise = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25
)



# ---------------------------------------- #
# ----    Automated Unit testing        ----
# ---------------------------------------- #

testthat::test_file("tests/testthat/test_RFunction.R")




# ---------------------------------------- #
# ----    MoveApps SDK testing          ----
# ---------------------------------------- #

# # Appending a csv file
# run_sdk(
#   data = test_inputs$input1, 
#   usr = usr, pwd = pwd, 
#   workflow_title = "Mock WF", 
#   app_title = "Add Local and Solar Time", 
#   product_file = "data_wtime")
# 
# output <- readRDS("data/output/output.rds"); output
# attr(output, "appended_products")
# 
# 
# # Appending a ctmm model object
# run_sdk(
#   data = test_inputs$input2, 
#   usr = usr, 
#   pwd = pwd, 
#   workflow_title = "mock",
#   app_pos = 12, 
#   product_file = "model")
# 
# output <- readRDS("data/output/output.rds"); output
