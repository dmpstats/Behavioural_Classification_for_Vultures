# Preamble ----------------------------
library(move2)
library(dplyr)
library(readr)
library(knitr)
library(MRSea)
library(httr2)
library(purrr)
library(glue)

options(tibble.width = Inf) 

# get App secret key for decrypting test dataset
source("tests/app-testing-helpers.r")
app_key <- get_app_key()

# Set-up version comparison
source("src/common/logger.R")
source("src/io/app_files.R")
source("dev/compare_versions.r")


# ggplot theme
theme_set(theme_light())

# Read (encrypted) input datasets for testing
test_dt <- secret_read_rds("data/raw/vult_test_data.rds", key = I(app_key))

test_dt$metadata |> kable(format = "markdown")

# thinning gaia and nam dataset for 5mins gap for faster testing. 
test_dt$gaia <- mt_filter_per_interval(test_dt$gaia, unit = "2 min")
test_dt$nam_sop <- mt_filter_per_interval(test_dt$nam_sop, unit = "2 min")



# Comparing versions after implementation of diagnostic plots -----------------

#' This involved addressing with instances where the algorithm behind the model
#' fitting fails to converge. In these cases, the speed threshold calculations
#' for the track in cause voided and thus the speed-time classification step is
#' skipped

source("dev/issue13-consolidating_speedtime_modelling/RFunction_curr.R")
source("dev/issue13-consolidating_speedtime_modelling/RFunction_diags.R")


compar_out <- imap(
  test_dt[names(test_dt) != "metadata"],
  \(dt, dt_name){
    
    message(paste0("\nPerforming comparison for dataset ", dt_name, "\n"))
    
    compare_versions(
      dt = dt,
      f_old = rFunction_curr, 
      f_new = rFunction_diags, 
      fun_new_label = "voided non-convergers", 
      fun_old_label = "original version",
      artifacts_path = "data/output/",
      travelcut = 3,
      create_plots = FALSE,
      sunrise_leeway = 0,
      sunset_leeway = 0,
      altbound = 25,
      keepAllCols = TRUE, 
      return_output = TRUE
    )}
)




map(compar_out, pluck(4)) |> 
  list_rbind(names_to = "dataset") |> 
  mutate(
    pctg_change = round(pctg_change, 1),
    change_sign = sign(pctg_change),
    change_sign = case_when(
      change_sign == 1 ~ "+",
      change_sign == -1 ~ "-",
      change_sign == 0 ~ ""
    ),
    change = glue::glue("{`original version`} -> {`voided non-convergers`} ({change_sign}{abs(pctg_change)}%)")
  ) |> 
  select(dataset, behav, change) |> 
  pivot_wider(names_from = behav, values_from = change) |> 
  kable(format = "markdown")





# Version comparison from changing hours-since-midnight to hours-since-sunrise -----------------

source("dev/issue13-consolidating_speedtime_modelling/RFunction_diags.R")
source("dev/issue13-consolidating_speedtime_modelling/RFunction_sunrise.R")

compar_out <- imap(
  test_dt[names(test_dt) != "metadata"],
  \(dt, dt_name){
    
    message(paste0("\nPerforming comparison for dataset ", dt_name, "\n"))
    
    compare_versions(
      dt = dt,
      f_old = rFunction_diags, 
      f_new = rFunction_sunrise, 
      fun_new_label = "hrs_since_sunrise", 
      fun_old_label = "voided non-convergers",
      artifacts_path = "data/output/",
      travelcut = 3,
      create_plots = FALSE,
      sunrise_leeway = 0,
      sunset_leeway = 0,
      altbound = 25,
      keepAllCols = TRUE, 
      return_output = TRUE
    )}
)


map(compar_out, pluck(4)) |> 
  list_rbind(names_to = "dataset") |> 
  mutate(
    pctg_change = round(pctg_change, 1),
    change_sign = sign(pctg_change),
    change_sign = case_when(
      change_sign == 1 ~ "+",
      change_sign == -1 ~ "-",
      change_sign == 0 ~ ""
    ),
    change = glue::glue("{`voided non-convergers`} -> {`hrs_since_sunrise`} ({change_sign}{abs(pctg_change)}%)")
  ) |> 
  select(dataset, behav, change) |> 
  pivot_wider(names_from = behav, values_from = change) |> 
  kable(format = "markdown")



# ------------------------------------------------------------------------------

work_dt <- imap(
  test_dt[names(test_dt) != "metadata"],
  \(dt, dt_name){
    
    message(paste0("\nRunning App for dataset ", dt_name, "\n"))
    
    rFunction(
      data = dt,
      travelcut = 3,
      create_plots = FALSE,
      sunrise_leeway = 0,
      sunset_leeway = 0,
      altbound = 25,
      keepAllCols = TRUE
    )}
)



dt_animal <- work_dt$pan_afr %>%
  mutate(ID = mt_track_id(.)) |>
  filter(ID == "Lizzy")

model_out <- speed_time_model(dt_animal)
mod_out <- speed_time_model_diag(dt_animal)



animal_dt <- rFunction(
  data = test_dt$sa_vfa |> filter(track == "TAWNY_8891..deploy_id.2187458663."),
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)

mod_out <- speed_time_model_sunrise(animal_dt)
mod_out <- speed_time_model(animal_dt)



test_1 <- dt %>%
  dplyr::mutate(
    month = month(timestamp),
    response = kmph + 0.00001
  ) %>%
  dplyr::filter(
    !is.na(response),
    stationary == 1,
    timestamp > (max(timestamp) - days(30))
  )

runSALSA1D(
  initialModel,
  salsa1dlist,
  varlist=c("hrs_since_sunrise"),
  splineParams=NULL,
  datain=test_1,
  predictionData = filter(dt, !is.na(kmph)),
  panelid = newdat$yearmonthday,
  suppress.printout = TRUE)$bestModel


test_2 <- dt %>%
  dplyr::mutate(
    month = month(timestamp),
    response = kmph + 0.00001
  ) %>%
  dplyr::filter(
    !is.na(response),
    stationary == 1,
    timestamp > (max(timestamp) - days(45))
  )


initialModel <- glm(response  ~ 1 , family = Gamma(link="log"), data = test_2)

runSALSA1D(
  initialModel,
  salsa1dlist,
  varlist=c("hrs_since_sunrise"),
  splineParams=NULL,
  datain=test_2,
  predictionData = filter(dt, !is.na(kmph)),
  panelid = newdat$yearmonthday,
  suppress.printout = TRUE)$bestModel










out <- rFunction(
  data = test_dt$savahn, # test_dt$wb_zam_knd,
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)


out <- rFunction(
  data = test_dt$nam_sop |> slice(1:1000), # test_dt$wb_zam_knd,
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)


out <- rFunction_diags(
  data = test_dt$nam_sop, # test_dt$wb_zam_knd,
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)


source("RFunction.R")

out_new <- rFunction(
  data = test_dt$nam_sop, # test_dt$wb_zam_knd,
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)


summary(out_new$hrs_since_sunrise)





 
# # Generate test-sets specific for this task - i.e. run app to get the stationary status
# 
# 
# 
# source("dev/issue13-consolidating_speedtime_modelling/RFunction_alt.R")
# 
# dt_animal <- work_dt$nam_sop %>%
#   mutate(ID = mt_track_id(.)) |>
#   filter(ID == "TO_6485")
# 
# model_out <- speed_time_model_alt(dt_animal)
# 
# 
# dt_animal <- work_dt$nam_sop %>%
#   mutate(ID = mt_track_id(.)) |>
#   filter(ID == "GA_5404")
# 
# model_out <- speed_time_model_alt(dt_animal)
# 
# dt_animal <- work_dt$pan_afr %>%
#   mutate(ID = mt_track_id(.)) |>
#   filter(ID == "TomPetty")
# 
# model_out <- speed_time_model_alt(dt_animal)
# 
# 
# out <- rFunction(
#   data = test_dt$gaia, # test_dt$wb_zam_knd,
#   travelcut = 3,
#   create_plots = FALSE,
#   sunrise_leeway = 0,
#   sunset_leeway = 0,
#   altbound = 25,
#   keepAllCols = TRUE
# )

# 
# dt_animal <- work_dt$gaia %>%
#   mutate(ID = mt_track_id(.)) |>
#   filter(ID == "V004")
# 
# model_out <- speed_time_model(dt_animal)







