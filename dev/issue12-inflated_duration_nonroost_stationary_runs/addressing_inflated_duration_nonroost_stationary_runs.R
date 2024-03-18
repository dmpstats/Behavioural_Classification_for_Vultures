library(move2)
library(dplyr)
library(readr)
library(knitr)

options(tibble.width = Inf) 

# get App secret key for decrypting test dataset
app_key <- get_app_key()

# Read (encrypted) input datasets for testing
test_dt <- secret_read_rds("data/raw/vult_test_data.rds", key = I(app_key))

test_dt$metadata


theme_set(theme_minimal())

source("src/common/logger.R")
source("src/io/app_files.R")


sel_cols <- c(
  "track", "timestamp", "sunrise_timestamp",  "sunset_timestamp", "yearmonthday", "timediff_hrs", 
  "dist_m", "kmph", "stationary", "stationaryNotRoost", "stationary_runLts", 
  "cumtimestat", "runtime", "dayRunThresh", "RULE", "behav"
)


# Comparison between versions ------------------------------------------------------------------
source("dev/compare_versions.r")
source("dev/issue12-inflated_duration_nonroost_stationary_runs/rFunction_old.r")
source("dev/issue12-inflated_duration_nonroost_stationary_runs/rFunction_statNotRoost_drop_lrg_Timediffs.R")

# thinning gaia and nam dataset for 5mins gap for faster testing. 
test_dt$gaia <- mt_filter_per_interval(test_dt$gaia, unit = "2 min")
test_dt$nam_sop <- mt_filter_per_interval(test_dt$nam_sop, unit = "2 min")


compar_out <- imap(
  test_dt[names(test_dt) != "metadata"],
  \(dt, dt_name){
    
    message(paste0("\nPerforming comparison for dataset ", dt_name, "\n"))
    
    compare_versions(
      dt = dt,
      f_old = rFunction_old, 
      f_new = rFunction_statNotRoost_drop_lrg_Timediffs, 
      fun_new_label = "drop_lrg_Timediffs_in_statNotRoost", 
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
    change = glue::glue("{`original version`} -> {drop_lrg_Timediffs_in_statNotRoost} ({change_sign}{abs(pctg_change)}%)")
    ) |> 
  select(dataset, behav, change) |> 
  pivot_wider(names_from = behav, values_from = change) |> 
  kable(format = "markdown")
  

# SA dataset ------------------------------------------------------------------
  
# Original version 
compar_out$sa_vfa$out_old |> 
  as_tibble() |> 
  slice(800:850) |> 
  dplyr::select(any_of(sel_cols)) |> 
  print(n = 200)


# re-creating data underpinning 
sa_eventtimes <- compar_out$sa_vfa$out_old %>% 
  mutate(ID = track) |> 
  as_tibble() %>%
  group_by(ID, stationary_runLts) %>%
  summarise(
    runtime = suppressWarnings(max(cumtimestat, na.rm = TRUE)),
    runtime = ifelse(is.infinite(runtime), 0, runtime), 
    .groups = "drop"
  ) %>%
  group_by(ID) %>% 
  mutate(dayRunThresh = quantile(runtime, probs = 0.95))


p1 <- sa_eventtimes |> 
  filter(ID == "Bateleur_8887..deploy_id.2145556232.") |> 
  ggplot() + 
  geom_histogram(aes(runtime), binwidth = 0.5, col = "darkblue", fill = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = unique(dayRunThresh)), linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    x = "Duration of a run of non-roosting stationary\nevents for the animal (`runtime`)", 
    title = "Dataset: South Africa vultures VfA MPIAB;  ID: Bateleur_8887..deploy_id.2145556232.",
    subtitle = "Red dashed line denotes `dayRunThresh`, the 95th percentile of stationary run durations", 
    caption = "Original Code Version"
  )

p1

ggsave(
  p1, 
  file = "dev/issue12-inflated_duration_nonroost_stationary_runs/hist_runtime_inflated_cumtimestat_example_1_old.png", 
  width = 8, height = 6
)



# Modified version 
compar_out$sa_vfa$out_new |> 
  as_tibble() |> 
  slice(800:850) |> 
  dplyr::select(any_of(sel_cols)) |> 
  print(n = 200)

# re-creating data underpinning 
sa_eventtimes_new <- compar_out$sa_vfa$out_new %>% 
  mutate(ID = track) |> 
  as_tibble() %>%
  group_by(ID, stationary_runLts) %>%
  summarise(
    runtime = suppressWarnings(max(cumtimestat, na.rm = TRUE)),
    runtime = ifelse(is.infinite(runtime), 0, runtime), 
    .groups = "drop"
  ) %>%
  group_by(ID) %>% 
  mutate(dayRunThresh = quantile(runtime, probs = 0.95))


p2 <- sa_eventtimes_new |> 
  filter(ID == "Bateleur_8887..deploy_id.2145556232.") |> 
  ggplot() + 
  geom_histogram(aes(runtime), binwidth = 0.5, col = "darkblue", fill = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = unique(dayRunThresh)), linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    x = "Duration of a run of non-roosting stationary\nevents for the animal (`runtime`)", 
    title = "Dataset: South Africa vultures VfA MPIAB;  ID: Bateleur_8887..deploy_id.2145556232.",
    subtitle = "Red dashed line denotes `dayRunThresh`, the 95th percentile of stationary run durations", 
    caption = "Modified Code Version"
  )

p2


ggsave(
  p2, 
  file = "dev/issue12-inflated_duration_nonroost_stationary_runs/hist_runtime_inflated_cumtimestat_example_1_new.png", 
  width = 8, height = 6
)


# pan_afr dataset ------------------------------------------------------------------

which(compar_out$pan_afr$out_old$behav != compar_out$pan_afr$out_new$behav)



# Original version 
compar_out$pan_afr$out_old |> 
  as_tibble() |> 
  slice(20:70) |> 
  dplyr::select(any_of(sel_cols)) |> 
  print(n = 200)

# re-creating data underpinning 
pan_afr_eventtimes_old <- compar_out$pan_afr$out_old %>% 
  mutate(ID = track) |> 
  as_tibble() %>%
  group_by(ID, stationary_runLts) %>%
  summarise(
    runtime = suppressWarnings(max(cumtimestat, na.rm = TRUE)),
    runtime = ifelse(is.infinite(runtime), 0, runtime), 
    .groups = "drop"
  ) %>%
  group_by(ID) %>% 
  mutate(dayRunThresh = quantile(runtime, probs = 0.95))


p3 <- pan_afr_eventtimes_old |> 
  filter(ID == "TomPetty") |> 
  ggplot() + 
  geom_histogram(aes(runtime), col = "darkblue", fill = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = unique(dayRunThresh)), linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    x = "Duration of a run of non-roosting stationary\nevents for the animal (`runtime`)", 
    title = "Dataset: pan-africa Vultures;  ID: TomPetty",
    subtitle = "Red dashed line denotes `dayRunThresh`, the 95th percentile of stationary run durations", 
    caption = "Original Code Version"
  )

p3

# Modified version 
compar_out$pan_afr$out_new |> 
  as_tibble() |> 
  slice(20:70) |> 
  dplyr::select(any_of(sel_cols)) |> 
  print(n = 200)

# re-creating data underpinning 
pan_afr_eventtimes_new <- compar_out$pan_afr$out_new %>% 
  mutate(ID = track) |> 
  as_tibble() %>%
  group_by(ID, stationary_runLts) %>%
  summarise(
    runtime = suppressWarnings(max(cumtimestat, na.rm = TRUE)),
    runtime = ifelse(is.infinite(runtime), 0, runtime), 
    .groups = "drop"
  ) %>%
  group_by(ID) %>% 
  mutate(dayRunThresh = quantile(runtime, probs = 0.95))


p4 <- pan_afr_eventtimes_new |> 
  filter(ID == "TomPetty") |> 
  ggplot() + 
  geom_histogram(aes(runtime), col = "darkblue", fill = "darkblue", alpha = 0.7) +
  geom_vline(aes(xintercept = unique(dayRunThresh)), linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    x = "Duration of a run of non-roosting stationary\nevents for the animal (`runtime`)", 
    title = "Dataset: pan-africa Vultures;  ID: TomPetty",
    subtitle = "Red dashed line denotes `dayRunThresh`, the 95th percentile of stationary run durations", 
    caption = "Modified Code Version"
  )

p4




