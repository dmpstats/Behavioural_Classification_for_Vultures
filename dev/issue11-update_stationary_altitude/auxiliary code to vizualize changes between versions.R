
# Preamble -----------------------------------------------
library(move2)
library(httr2)
library(purrr)
library(dplyr)
library(readr)
library(knitr)
library(ggplot2)

source("tests/app-testing-helpers.r")
source("dev/issue11-update_stationary_altitude/compare_versions.r")

# get App secret key for decrypting test dataset
app_key <- get_app_key()

source("src/common/logger.R")
source("src/io/app_files.R")

options(pillar.width = 500)


# Read (encrypted) input datasets for testing
test_dt <- secret_read_rds("data/raw/vult_test_data.rds", key = I(app_key))


# Roosting site relocation  -----------------------------------------------
#
#' Looking at example cases from a Namibia dataset (only for the 1st 3000 events)

source("dev/issue11-update_stationary_altitude/RFunction_roost_reloc.R")
source("dev/issue11-update_stationary_altitude/RFunction_old.R")

compare_out <- compare_versions(
  dt = test_dt$nam_sop |> slice(1:7000),
  f_old = rFunction_old, 
  f_new = rFunction_roost_reloc, 
  return_output = TRUE, 
  fun_new_label = "roost_reloc",
  artifacts_path = "data/output/",
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)


which(compare_out$out_new$behav != compare_out$out_old$behav)


sel_cols <- c("track", "timestamp", "kmph", "dist_m", "sunrise_timestamp", "sunset_timestamp", 
              "stationary", "altchange", "nightpoint", "endofday", "revcum_trav", 
              "roostgroup", "RULE", "behav")

## Type-case I: altered code delays start of long roosting run

### example 1
compare_out$out_old |> 
  as_tibble() |>
  slice(415:470) |> 
  dplyr::select(any_of(sel_cols))|> 
  print(n = 300)


compare_out$out_new |> 
  as_tibble() |>
  slice(415:470) |> 
  dplyr::select(any_of(sel_cols))|> 
  print(n = 300)


## Type-case II: altered impacts threshold for unusually long non-roosting stationary time

sel_cols <- c("track", "timestamp", "kmph", "sunrise_timestamp", "sunset_timestamp", 
              "stationary", "altchange", "nightpoint", "endofday", "revcum_trav", 
              "roostgroup", "cumtimestat", "dayRunThresh", "RULE", "behav")

compare_out$out_old |> 
  as_tibble() |>
  slice(1540:1570) |> 
  dplyr::select(any_of(sel_cols))|> 
  print(n = 300)

compare_out$out_new |> 
  as_tibble() |>
  slice(1540:1570) |> 
  dplyr::select(any_of(sel_cols))|> 
  print(n = 300)



## Type-case III: altered code anticipates end of roosting run

sel_cols <- c("track", "timestamp", "kmph", "dist_m", "sunrise_timestamp", "sunset_timestamp", 
              "stationary", "altchange", "nightpoint", "endofday", "revcum_trav", 
              "roostgroup", "RULE", "behav")

# compare_out$out_old |> as_tibble() |> slice(3010:3240) |> dplyr::select(any_of(sel_cols)) |> head(n = 30) |> print(n = 30)
# compare_out$out_old |> as_tibble() |> slice(3010:3240) |> dplyr::select(any_of(sel_cols)) |>  tail(n = 30) |> print(n = 30)
#   
 
# compare_out$out_new |> as_tibble() |> slice(3010:3240) |> dplyr::select(any_of(sel_cols)) |> head(n = 30) |> print(n = 30)
# compare_out$out_new |> as_tibble() |> slice(3010:3240) |> dplyr::select(any_of(sel_cols)) |>  tail(n = 30) |> print(n = 30)


compare_out$out_old |> 
  as_tibble() |>
  slice(5700:5760) |> 
  dplyr::select(any_of(sel_cols))|> 
  print(n = 300)

compare_out$out_new |> 
  as_tibble() |>
  slice(5700:5760) |> 
  dplyr::select(any_of(sel_cols))|> 
  print(n = 300)


# Speed-time model fitted to stationary == 1  -----------------------------------------------

source("dev/issue11-update_stationary_altitude/RFunction_spdtm_reloc.r")
source("dev/issue11-update_stationary_altitude/RFunction_old.R")

compare_out <- compare_versions(
  dt = test_dt$nam_sop,
  f_old = rFunction_old, 
  f_new = rFunction_spdtm_reloc, 
  return_output = TRUE, 
  fun_new_label = "roost_reloc",
  artifacts_path = "data/output/",
  travelcut = 3,
  create_plots = FALSE,
  sunrise_leeway = 0,
  sunset_leeway = 0,
  altbound = 25,
  keepAllCols = TRUE
)


which(compare_out$out_new$behav != compare_out$out_old$behav)

sel_cols <- c("track", "timestamp", "kmph", "dist_m", "sunrise_timestamp", "sunset_timestamp", 
              "stationary", "altchange", "nightpoint", "endofday", "roostgroup", 
              "kmphCI2.5", "kmphpreds", "kmphCI97.5", "RULE", "behav")


# compare_out$out_old |> 
#   as_tibble() |>
#   slice(70:125) |> 
#   dplyr::select(any_of(sel_cols))|> 
#   print(n = 300)
# 
# 
# compare_out$out_new |> 
#   as_tibble() |>
#   slice(70:125) |> 
#   dplyr::select(any_of(sel_cols))|> 
#   print(n = 300)
# 
# 
# 
# compare_out$out_old |> 
#   as_tibble() |>
#   slice(1275:1310) |> 
#   dplyr::select(any_of(sel_cols))|> 
#   print(n = 300)
# 
# compare_out$out_new |> 
#   as_tibble() |>
#   slice(1275:1310) |> 
#   dplyr::select(any_of(sel_cols))|> 
#   print(n = 300)


compare_out$out_old |> 
  as_tibble() |>
  mutate(kmphpreds = as.vector(kmphpreds)) |> 
  slice(36615:36640) |> 
  dplyr::select(any_of(sel_cols))|> 
  print(n = 300)

compare_out$out_new |> 
  as_tibble() |>
  mutate(kmphpreds = as.vector(kmphpreds)) |> 
  slice(36615:36640) |> 
  dplyr::select(any_of(sel_cols))|> 
  print(n = 300)


theme_set(theme_light())

hist_p <- compare_out$out_old |> 
  filter(kmph < 3) |> 
  mutate(is_travelling = ifelse(behav == "STravelling", TRUE, FALSE)) |> 
  ggplot2::ggplot(aes(x = kmph, fill = is_travelling, col = is_travelling)) +
  geom_histogram(alpha = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") 

ggsave(
  plot = hist_p,
  filename = "dev/issue11-update_stationary_altitude/hist_speed_below_travelcut_travel_vs_nontravel.png", 
  width = 7,
  height = 6)


compare_out$out_old |> 
  filter(kmph < 3) |> 
  mutate(is_travelling = ifelse(behav == "STravelling", TRUE, FALSE)) |> 
  ggplot2::ggplot(aes(x = hourmin, y = kmph, fill = is_travelling, col = is_travelling)) +
  geom_point(alpha = 0.2) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~is_travelling)


