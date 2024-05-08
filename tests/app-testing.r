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
test_dt$gaia <- mt_filter_per_interval(test_dt$gaia, unit = "5 min")
test_dt$nam_sop <- mt_filter_per_interval(test_dt$nam_sop, unit = "5 min")


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


mod_out <- speed_time_model(
  dt = track_dt |> filter(track == "AW196500..deploy_id.2023814797."), 
  model_obj = TRUE, 
  void_non_converging = TRUE)

mod_out <- speed_time_model(
  dt = track_dt |> filter(track == "AW196499..deploy_id.2023814814."), 
  model_obj = TRUE, 
  void_non_converging = TRUE)


mod_out <- speed_time_model(track_dt |> filter(track == "A33506..deploy_id.1935590052."), model_obj = TRUE, void_non_converging = TRUE)

mod_out <- speed_time_model(track_dt |> filter(track == "A234556..deploy_id.2153340562."), model_obj = TRUE, void_non_converging = TRUE)

mod_out <- speed_time_model(track_dt |> filter(track == "A116514..deploy_id.1935590050."), model_obj = TRUE, void_non_converging = TRUE)


  


# nam_sop: "GA_6594" (runSALSA1D error)
# savanha: "SAV.4360.A" (runSALSA1D error)
# ken_tnz: "C175593" (runSALSA1D error); "A234556" (non-convergence)
# wcs: "AW196499..deploy_id.2023814814.", "B175706..deploy_id.2023814800." (non-convergence)
# wb_zam_knd: "A234556..deploy_id.2153340562." (non-convergence); "X163116Z..deploy_id.2388610874" (runSALSA1D error)

lindesay_dt <- test_dt[!names(test_dt) %in% c("metadata", "gaia", "pan_afr", "sa_vfa")]
lindesay_dt$nam_sop <- lindesay_dt$nam_sop |> filter(track == "GA_6594")
lindesay_dt$savahn <- lindesay_dt$savahn |> filter(track == "SAV.4360.A")
lindesay_dt$ken_tnz <- lindesay_dt$ken_tnz |> filter(track %in% c("C175593", "A234556"))
lindesay_dt$wcs <- lindesay_dt$wcs |> filter(track %in% c("AW196499..deploy_id.2023814814.", "B175706..deploy_id.2023814800."))
#lindesay_dt$wb_zam_knd <- lindesay_dt$wb_zam_knd |> filter(track %in% c("A234556..deploy_id.2153340562.", "X163116Z..deploy_id.2388610874"))

# use this to reload Rfunction with browser
set_interactive_app_testing()

# ~~~~~~~~~~~~~~~~~~
# TANZANIA
# ~~~~~~~~~~~~~~~~~~
track_dt <- rFunction(
  data = lindesay_dt$ken_tnz,
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)

# trying a slightly different time range...
track_dt_sub <- rFunction(
  data = filter(lindesay_dt$ken_tnz, timestamp < max(timestamp)-days(20)),
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)

mod_out <- speed_time_model(track_dt |> filter(track == "C175593"), model_obj = TRUE, void_non_converging = TRUE)
# salsa error if use full data
# change to gaussian with log link and fits fine, however, mean var is not good as variance increases with mean....

mod_out <- speed_time_model(track_dt_sub |> filter(track == "C175593"), model_obj = TRUE, void_non_converging = TRUE)
# model not converged but not classed as NULL by p-values
# no values outside CIs so no changes
# seems like a fairly immobile bird
# change to gaussian "fixes"issues

# 

mod_out <- speed_time_model(track_dt |> filter(track == "A234556"), model_obj = TRUE, void_non_converging = TRUE)
# not converged, fit changed to NULL
# gaussian works but knot really close to left boundary, 
# changed to have gap=0.5 --> half an hour between knots

mod_out <- speed_time_model(track_dt_sub |> filter(track == "A234556"), model_obj = TRUE, void_non_converging = TRUE)
# gaussian works but very uncertain in last part (13hrs post sunrise). not really an issue



# ~~~~~~~~~~~~~~~~~~
# WCS
# ~~~~~~~~~~~~~~~~~~
# wcs: "AW196499..deploy_id.2023814814.", "B175706..deploy_id.2023814800." (non-convergence)
set_interactive_app_testing()

track_dt <- rFunction(
  data = lindesay_dt$wcs,
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)

# trying a slightly different time range...
track_dt_sub <- rFunction(
  data = filter(lindesay_dt$wcs, timestamp < max(timestamp)-days(20)),
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)

mod_out <- speed_time_model(track_dt |> filter(track == "AW196499..deploy_id.2023814814."), model_obj = TRUE, void_non_converging = TRUE)
# model not converged with gamma
# change to gaussian with log link and fits fine, however, mean var is not good as variance increases with mean....

mod_out <- speed_time_model(track_dt_sub |> filter(track == "AW196499..deploy_id.2023814814."), model_obj = TRUE, void_non_converging = TRUE)
# model not converged with gamma
# change to gaussian "fixes"issues

# 

mod_out <- speed_time_model(track_dt |> filter(track == "B175706..deploy_id.2023814800."), model_obj = TRUE, void_non_converging = TRUE)
# not converged, fit changed to NULL
# gaussian works 

mod_out <- speed_time_model(track_dt_sub |> filter(track == "B175706..deploy_id.2023814800."), model_obj = TRUE, void_non_converging = TRUE)
# not converged, fit changed to NULL
# gaussian not converged either, so no model fitted...
# note that with date cut this is only 14 days of data...


# ~~~~~~~~~~~~~~~~~~
# SAVANNAH
# ~~~~~~~~~~~~~~~~~~
# savanha: "SAV.4360.A" (runSALSA1D error)

set_interactive_app_testing()

track_dt <- rFunction(
  data = lindesay_dt$savahn,
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)

mod_out <- speed_time_model(track_dt |> filter(track == "SAV.4360.A"), model_obj = TRUE, void_non_converging = TRUE)
# fine to fail, this tag is not on a bird!!
# sitting in Gabes house
# fixed by changing to 5 fold CV. uses gamma

# ~~~~~~~~~~~~~~~~~~
# NAMIBIA SOP
# ~~~~~~~~~~~~~~~~~~
# nam_sop: "GA_6594" (runSALSA1D error)

set_interactive_app_testing()

track_dt <- rFunction(
  data = lindesay_dt$nam_sop,
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)

mod_out <- speed_time_model(track_dt |> filter(track == "GA_6594"), model_obj = TRUE, void_non_converging = TRUE)
# neither model any good - k-fold issue
# only 14 days of data
# fixed by changing k-folds to 5. fits using gamma

# ~~~~~~~~~~~~~~~~~~
# Zambia birds
# ~~~~~~~~~~~~~~~~~~
# wb_zam_knd: "A234556..deploy_id.2153340562." (non-convergence); "X163116Z..deploy_id.2388610874" (runSALSA1D error)

set_interactive_app_testing()

track_dt <- rFunction(
  data = lindesay_dt$wb_zam_knd,
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)

mod_out <- speed_time_model(track_dt |> filter(track == "A234556..deploy_id.2153340562."), model_obj = TRUE, void_non_converging = TRUE)
# gamma issue but fits with log gaussian

mod_out <- speed_time_model(track_dt |> filter(track == "X163116Z..deploy_id.2388610874"), model_obj = TRUE, void_non_converging = TRUE)
# not enough data




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  TANZANIA full test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tanz_dt2 <- readRDS(file = "C:\\Users\\lindesay\\University of St Andrews\\MLM_LSHjointwork - Documents\\research\\NCZoo\\Clustering_Callum\\data\\rawdata\\Kendall_and_WCS_combine__Workflow_Product_Retriever__2024-03-25_12-58-03.rds")

set_interactive_app_testing()

for(i in 1:length(unique(tanz_dt2$individual_name_deployment_id))){
  
  track_dt <- rFunction(
    data = filter(tanz_dt2, individual_name_deployment_id==unique(tanz_dt2$individual_name_deployment_id)[i]),
    travelcut = 3,
    create_plots = TRUE,
    sunrise_leeway = 0,
    sunset_leeway = 0,
    altbound = 25,
    keepAllCols = FALSE
  )
}

track_dt <- rFunction(
  data = filter(tanz_dt2),
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)


tanz_dt <- readRDS(file = "C:\\Users\\lindesay\\University of St Andrews\\MLM_LSHjointwork - Documents\\research\\NCZoo\\Clustering_Callum\\data\\outputdata\\1)Combined_standardised_TZ_11012024.rds")

set_interactive_app_testing()

track_dt <- rFunction(
  data = filter(tanz_dt),
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)




debugonce(speed_time_model)
mod_out <- speed_time_model(track_dt |> filter(track == "C181261"), model_obj = TRUE, void_non_converging = TRUE)

tanz_dt_sub <- tanz_dt %>%
  filter(track=="A181267") %>% 
  filter(timestamp > max(timestamp)- days(100))

track_dt_sub <- rFunction(
  data = tanz_dt_sub,
  travelcut = 3,
  create_plots = TRUE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = FALSE
)

birdplot_sub <- track_dt_sub |> 
  ggplot(aes(x = sf::st_coordinates(track_dt_sub)[, 1], y = sf::st_coordinates(track_dt_sub)[, 2]) ) +
  geom_path(col = "gray80") +
  geom_point(aes(colour = behav)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = paste0("Behaviour classification for track ID "),
    x = "Easting", y = "Northing"
  )

track_dt_full <- filter(track_dt, timestamp > max(timestamp)- days(100))

birdplot1 <- track_dt_full |> 
  ggplot(aes(x = sf::st_coordinates(track_dt_full)[, 1], y = sf::st_coordinates(track_dt_full)[, 2]) ) +
  geom_path(col = "gray80") +
  geom_point(aes(colour = behav)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = paste0("Behaviour classification for track ID "),
    x = "Easting", y = "Northing"
  )

table(track_dt_full$behav)
table(track_dt_sub$behav)






