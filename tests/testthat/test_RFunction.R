library(move2)
library(withr)
library(dplyr)
library(rlang)
library(httr2)


if(rlang::is_interactive()){
  library(testthat)
  source("tests/app-testing-helpers.r")
  set_interactive_app_testing()
  app_key <- get_app_key()
}

test_sets <- httr2::secret_read_rds(test_path("data/unit_test_sets.rds"), key = I(app_key))



test_that("Input validation is working", {
  
  # invalid altbound
  expect_error(
    rFunction(
      data = test_sets$sa_vfa_15days, 
      travelcut = 3, 
      altbound = -18, 
      create_plots = FALSE), 
    regexp = "Invalid altitude change threshold \\(`altbound`\\). Please provide values >= 0."
  )
  
  # invalid travelcut 
  expect_error(
    rFunction(
      data = test_sets$sa_vfa_15days, 
      travelcut = -7, 
      create_plots = FALSE), 
    regexp = "Invalid speed upper-bound for stationary behavour \\(`travelcut`\\). Please provide values > 0."
  )
  
  expect_error(
    rFunction(
      data = test_sets$sa_vfa_15days, 
      travelcut = 0, 
      create_plots = FALSE), 
    regexp = "Invalid speed upper-bound for stationary behavour \\(`travelcut`\\). Please provide values > 0."
  )
  
  
  # missing essential columns
  expect_error(
    rFunction(
      data = test_sets$sa_vfa_15days |> dplyr::select(-sunrise_timestamp), 
      travelcut = 3, 
      create_plots = FALSE), 
    regexp = "`sunrise_timestamp` and/or `sunset timestamp` are not a columns in the input data"
  )
  
  expect_error(
    rFunction(
      data = test_sets$sa_vfa_15days |> dplyr::select(-timestamp_local), 
      travelcut = 3), 
    regexp = "Column `timestamp_local` is not comprised in input data. Local time is a fundamental requirement"
  )
  
})





test_that("Expected classification outcome has not changed", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  
  local_edition(3)
  
  expect_snapshot_value(
    rFunction(data = test_sets$savahn_tiny, create_plots = FALSE) |> 
      dplyr::as_tibble() |> 
      dplyr::count(behav), 
    style = "deparse"
  )
  
  
  expect_snapshot_value(
    rFunction(data = test_sets$ken_tnz, create_plots = FALSE) |> 
      dplyr::as_tibble() |> 
      dplyr::count(behav), 
    style = "deparse"
  )
  
  expect_snapshot_value(
    table(rFunction(data = test_sets$sa_vfa_15days, create_plots = FALSE)$behav), 
    style = "deparse"
  )
  
})



# test_that("all behaviours are classified", {
#   
#   actual <- rFunction(data = test_data, travelcut = 3, create_plots = FALSE)
#     
#   
#   expect(nrow(actual) == nrow(test_data),
#          failure_message = "Locations have been lost during classification. No locations should be removed")
#   expect(unique(mt_track_id(actual)) == unique(mt_track_id(test_data)), 
#          failure_message = "IDs have been lost during classification")
# 
#   })
# 
#
# 
# test_that("expected classification outcome has not changed", {
#   
#   actual <- rFunction(data = test_data,
#                       rooststart = 18, 
#                       roostend = 7,
#                       travelcut = 3)
#   
#   
#   expect(sum(actual$behav == "SFeeding") == 115, 
#          failure_message = "Number of feeding locations has changed unexpectedly without methodology change")
#   
# })
