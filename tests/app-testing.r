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


# thinning gaia and nam dataset for 5mins gap for faster testing. 
test_dt$gaia <- mt_filter_per_interval(test_dt$gaia, unit = "2 min")
test_dt$nam_sop <- mt_filter_per_interval(test_dt$nam_sop, unit = "2 min")


# ---------------------------------------- #
# ----   Interactive RFunction testing  ----
# ---------------------------------------- #

set_interactive_app_testing()

out_cur <- rFunction(
  data =  test_dt$sa_vfa, #test_dt$wb_zam_knd, #test_dt$gaia, #test_dt$savahn, test_dt$sa_vfa test_dt$nam_sop
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)

count(out_cur, behav)


out <- rFunction(
  data = test_dt$gaia,
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)

out <- rFunction(
  data = test_dt$nam_sop,
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)


out <- rFunction(
  data = test_dt$savahn,
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)


out <- rFunction(
  data = test_dt$wb_zam_knd,
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)


out <- rFunction(
  data = test_dt$pan_afr,
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)


out <- rFunction(
  data = test_dt$ken_tnz,
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)

out <- rFunction(
  data = test_dt$wcs,
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)




# ---------------------------------------- #
# ----    Automated Unit testing        ----
# ---------------------------------------- #

testthat::test_file("tests/testthat/test_RFunction.R")




# ---------------------------------------- #
# ----    MoveApps SDK testing          ----
# ---------------------------------------- #

# standard dataset with default inputs
run_sdk(
  data = test_dt$sa_vfa, 
  travelcut = 3, 
  create_plots = TRUE, 
  sunrise_leeway = 0, 
  sunset_leeway = 0, 
  altbound = 25, 
  keepAllCols =  TRUE)

output <- readRDS("data/output/output.rds"); output
table(output$behav)



# different dataset and inputs
run_sdk(
  data = test_dt$savahn, 
  travelcut = 10, 
  create_plots = TRUE, 
  sunrise_leeway = 30, 
  sunset_leeway = 30, 
  altbound = 10, 
  keepAllCols =  TRUE)

output <- readRDS("data/output/output.rds"); output
table(output$behav)


# no plots
run_sdk(
  data = test_dt$ken_tnz, 
  travelcut = 3, 
  create_plots = FALSE, 
  sunrise_leeway = -5, 
  sunset_leeway = 5, 
  altbound = 20, 
  keepAllCols = FALSE)





# ---------------------------------------- #
# ----    Speed-time model testing     ----
# ---------------------------------------- #


track_dt <- rFunction(
  data = test_dt$wcs,
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)

mod_out <- speed_time_model(track_dt |> filter(track == "AW196500..deploy_id.2023814797."), model_obj = TRUE, void_non_converging = TRUE)
mod_out <- speed_time_model(track_dt |> filter(track == "AW196499..deploy_id.2023814814."), model_obj = TRUE, void_non_converging = TRUE)

track_dt |> filter(track == "AW196499..deploy_id.2023814814.") |> 
  filter(stationary == 1) |> 
  ggplot(aes(y = kmph, x = hrs_since_sunrise )) +
  geom_point()





