library('move2')
library('lubridate')
library('magrittr')
library('dplyr')
library('ggplot2')
library('data.table')
library('sf')
library('units')
library('stringr')
library('tidyr') 
library('MRSea')
library("purrr")
library("zoo")
library("spatstat.utils")
library("furrr")
library("future")
library("progressr")
library("patchwork")
library("splines")
library("rlang")
library("grid")

`%!in%` <- Negate(`%in%`)
not_null <- Negate(is.null)


# Main RFunction ====================================================================

rFunction = function(data, 
                     travelcut = 3,
                     create_plots = TRUE,
                     sunrise_leeway = 0,
                     sunset_leeway = 0,
                     altbound = 25,
                     keepAllCols = FALSE) {
  
  
  #' TODO (Desirables)
  #' 
  #'   - make use of 'dplyr::' consistent
  #'   - drop "ID" and "timestamp" redefinition and use "mt_" functions instead
  #'   - improve error messages with {rlang}
  #'   - add timestamps to logger
  
  
  ## Globals --------------------------------------
  ggplot2::theme_set(ggplot2::theme_bw())
  
  
  ## Validate Input Data --------------------------------------------
  
  logger.trace(paste0(
    "\nInput data provided:  \n", 
    "travelcut: ", toString(travelcut), "\n",
    "sunrise_leeway: ", toString(sunrise_leeway), "\n", 
    "sunset_leeway: ", toString(sunset_leeway), "\n",
    "altbound: ", toString(altbound), "\n",
    "input data dimensions: ", toString(dim(data))
  ))
  
  logger.info("Starting input validation")
  
  if(nrow(data) == 0) {
    logger.warn("Input data is empty. Returning input.")
    return(data)}
  
  
  ### travelcut ----
  if(travelcut <= 0) {
    logger.fatal("Speed cut-off for travelling behavour must be > 0. Terminating App.")
    stop("Invalid speed cut-off for travelling behavour (`travelcut`). Please provide values > 0.")
  }
  
  
  ### altbound ----
  ### (only if column named "altitude" is in input dataset)
  
  if("altitude" %in% colnames(data)){
    
    if(altbound < 0) {
      logger.fatal("`altbound` must be >= 0. Terminating computation.")
      stop("Invalid altitude change threshold (`altbound`). Please provide values >= 0.")
      
    } else if(altbound == 0){
      
      logger.warn(
        paste0(" |- Altitude threshold (`altbound`) is set to 0m, and thus ",
               "ANY change is altitude will be considered as ascencing/descending ",
               "movement.")
      )
    }  
  } 
  
  ### 'leeway' inputs ----
  if(is.null(sunrise_leeway)) {
    logger.warn(" |- No sunrise leeway provided as input. Defaulting to no leeway.")
    sunrise_leeway <- 0
  }
  if(is.null(sunset_leeway)) {
    logger.warn(" |- No sunset leeway provided as input. Defaulting to no leeway.")
    sunset_leeway <- 0
  }
  
  
  ### Input data: time-related columns -----
  if("altitude" %!in% colnames(data)){
    logger.warn(" |- Column `altitude` is absent from input data.")
  } else{
    logger.info(" |- `altitude` column identified. Able to detect altitude changes.")
  }
  
  
  if ("timestamp_local" %!in% colnames(data)) {
    logger.fatal(" |- `timestamp_local` is not comprised in input data. Terminating App execution.")
    stop(
      paste0(
        "Column `timestamp_local` is not comprised in input data. Local time is ",
        "a fundamental requirement for the classification process.\n",   
        "   Please deploy the App 'Add Local and Solar Time' earlier in the Workflow ",  
        "to bind local time to the input dataset."),
      call. = FALSE
    )
  }else{
    logger.info(" |- Local Time column identified")
  }
  
  
  if ("sunrise_timestamp" %!in% colnames(data) | "sunset_timestamp" %!in% colnames(data)) {
    logger.fatal("`sunrise_timestamp` and/or `sunset timestamp` columns are missing in the input data. Terminating App.")
    stop(
      paste0(
        "`sunrise_timestamp` and/or `sunset timestamp` are not a columns in the ",
        "input data.\n   Identification of night-time points is fundamental for the ",
        "classification process. Please deploy the App 'Add Local and Solar Time' ",
        "earlier in the workflow to add the required columns."), 
      call. = FALSE
    )
  } else {
    logger.info(" |- Sunrise and sunset columns identified. Able to perform night-time identification.")
  }
  
  
  
  logger.info(" |- Input is in correct format. Proceeding with data preparation.")
  
  
  ## Data Preparation ===========================================================
  
  logger.info("Initiate Data Preparation Steps")

  
  ### Generate general variables  ------------------------------

  logger.info(" |- Generate general variables")
  
  data %<>% 
    dplyr::mutate(
      ID = mt_track_id(.),
      timestamp = mt_time(.)
    ) %>%
    # drop events with missing timestamps
    dplyr::filter(!is.na(timestamp)) %>%
    # order by time within track
    arrange(ID, timestamp)
    # distinct(timestamp, .keep_all = TRUE)
  
  
  # Add date label, day hours-since-midnight and hours-since-sunrise
  data %<>% 
    mutate(
      yearmonthday = stringr::str_replace_all(stringr::str_sub(timestamp_local, 1, 10), "-", ""),
      hourmin = lubridate::hour(timestamp_local) + 
        lubridate::minute(timestamp_local)/60 + 
        lubridate::second(timestamp_local)/3600,
      hrs_since_sunrise = 
        as.double(
          difftime(
            lubridate::with_tz(timestamp, lubridate::tz(sunrise_timestamp)), # ensures TZ consistency
            sunrise_timestamp, 
            units = "hour"
          ))
    )
  
  
  #' NOTE:`timediff_hrs`, `dist_m` & `kmph` are variables expected to provide
  #' information between consecutive locations. If the Standardizing App (or
  #' other) has been used earlier in the WF, these cols could already be present
  #' in the input. However, there is no guarantee that data coming as input has
  #' been thinned by other in-between App. Therefore, to ensure accuracy, we
  #' always (re)generate these columns here.
  data %<>% 
    mutate(
      timediff_hrs = as.vector(mt_time_lags(., units = "hours")),
      kmph = as.vector(mt_speed(., units = "km/h")),
      dist_m = as.vector(mt_distance(., units = "m"))
    ) 
  

  ### Identify stationary points -----------------------------------
  
  logger.info(" |- Identify stationary points")
  
  #' Identify stationary points
  #' - events with speed <= travelcut == stationary (1),
  #' - events where speed > travelcut == non-stationary (0),
  #' - events with NA speed == stationary (1)

  data %<>%
    mutate(
      stationary = case_when(
        kmph <= travelcut ~ 1,
        kmph > travelcut ~ 0,
        is.na(kmph) | is.nan(kmph) ~ 1 # assume stationary if no data
      )
    )
  
  
  
  ### Detect Altitude Changes -------------------------------------
  
  #' Categorize vertical movement based on altitude change
  #'  (i) change in altitude to next location > threshold: altchange == "ascent"
  #'  (ii) change in altitude to next location < -threshold: altchange == "descent"
  #'  (iii) else (including NAs): altchange == "flatline"
  
  if ("altitude" %in% colnames(data)) {
    
    if(!all(is.na(data$altitude))){
      
      logger.info(" |- Categorize altitude change between consecutive locations")
      
      # Classify altitude changes
      data %<>% 
        # Reset altitude change each day:
        group_by(ID, yearmonthday) %>%
        dplyr::mutate(
          altitude = as.numeric(altitude), # fix when input is character vector
          
          altdiff = dplyr::lead(altitude) - altitude,
          
          altchange = case_when(
            altdiff < -altbound ~ "descent",
            altdiff > altbound ~ "ascent",
            .default = "flatline"
          )) %>% 
        ungroup()
      
      alt_classify <- TRUE
      
    } else{
      alt_classify <- FALSE  
    }
  } else {
    alt_classify <- FALSE  
  }
  
  if(!alt_classify){
    logger.warn(
      paste0(" |- Column `altitude` is either not present in input data or is entirely ",
             "filled with NAs - skipping altitude change calculations."))
  }
  
  
  
  ### Night-time identification ----------------------------

  #' (i) nightpoint == 0 if sunrise_timestamp < timestamp < sunrise_timestamp (+/- leeway), 
  #' (ii) otherwise nightpoint == 1
  
  logger.info(" |- Identify night-time locations")

  data %<>% mutate(
    nightpoint = ifelse(
      between(
        lubridate::with_tz(timestamp, lubridate::tz(sunrise_timestamp)), # with_tz() ensures TZs consistency
        sunrise_timestamp + lubridate::minutes(sunrise_leeway), 
        sunset_timestamp + lubridate::minutes(sunset_leeway)
      ), 
      0, 1)
    )
  
  
  
  ### Calculate ACC variance ----------------------------
  
  #' If ACC data is available, calculate variance in acceleration bursts
  #' till subsequent location event - expect one variance statistic for each enabled ACC axes
  
  if ("acc_dt" %in% colnames(data)) {
    
    # logical flag for all-NULL 'acc_dt' column 
    acc_null <- all(purrr::map_lgl(data$acc_dt, is.null))
    
    if(!acc_null){
      logger.info(" |- ACC data identified: calculating variance in acceleration between locations.")
      
      # Unnest ACC data and compute ACC variance between consecutive locations
      data <- acc_var(data, interpolate = FALSE) |> 
        dplyr::select(-acc_dt)
      
      ACCclassify <- TRUE
      
    } else {
      data <- dplyr::select(data, -acc_dt)
      ACCclassify <- FALSE
    }
  } else{
    ACCclassify <- FALSE
  }  
  
  if(!ACCclassify) logger.info(" |- No accelerometer data detected in any of the tracks: skipping ACC preparation.")
  

  
  # Behaviour Classification Steps [1 -7] ========================================================
  
  logger.info("All data prepared. Performing all classification steps")
  
  
  
  ### [1] Speed Classification -------------------
  
  logger.info("[1] Performing speed classification")
  data %<>% mutate(
    # Add column to explain classification:
    RULE = ifelse(stationary == 1, "[1] Low speed","[1] High speed"), 
    behav = ifelse(stationary == 1, "SResting", "STravelling")
  )
  
  # Log results
  logger.info(paste0("   |> ", sum(data$behav == "SResting", na.rm = T), " locations classified as SResting"))
  logger.info(paste0("   |> ", sum(data$behav == "STravelling", na.rm = T), " locations classified as STravelling"))
  
  
  
  
  ### [2] Altitude Classification --------------------
  
  #' Remaining resting locations reclassified as travelling according to the following rules:
  #' (i) If a bird is ascending ==> STravelling
  #' (ii) If a bird is descending AND:
  #'      Next location is ascending/descending ==> STravelling
  #'      Next location is flatlining ==> remains SResting
  #' (iii) If a bird is flatlining, it remains SResting
  
  if(alt_classify){
  
    logger.info("[2] Performing altitude classification")
    
    data %<>%
      # QUESTION (BC): shouldn't this step be grouped by bird given we're using `lead()`?
      # group_by(ID) %>%
      mutate(
        RULE = case_when(
          (behav == "SResting") & (altchange == "ascent") ~ "[2] Altitude increasing",
          (behav == "SResting") & (altchange == "descent") & (lead(altchange) %in% c("descent", "ascent")) ~ "[2] Altitude decreasing",
          TRUE ~ RULE
        ),
        behav = case_when(
          (behav == "SResting") & (altchange == "ascent") ~ "STravelling",
          (behav == "SResting") & (altchange == "descent") & (lead(altchange) %in% c("descent", "ascent")) ~ "STravelling",
          TRUE ~ behav
        )
      )
    
    #### <!> Update stationary status -------------
    data <- data |> mutate(stationary = ifelse(behav == "STravelling", 0, stationary))
    
    # Log results
    logger.info(paste0("   |> ", sum(data$RULE == "[2] Altitude increasing" | data$RULE == "[2] Altitude decreasing", na.rm = T), " locations re-classified as STravelling"))  
  
  } else {
    logger.warn("[2] Skipping altitude classification due to absence of altitude data")
  }
  
  
  
  
  ### [3] Night-time Classification ---------------
  
  #' Remaining resting locations re-classified as (night-time) roosting if they've 
  #' been identified as a night point (i.e. occurred between sunset and sunrise)
  #' 
  #' NOTE: STravelling locations are kept unchanged, i.e. night-time travelling 
  #' treated as a valid behaviour
  
  logger.info("[3] Performing night-time classification")
  
  data %<>%
    mutate(
      RULE = case_when(
        (behav == "SResting") & (nightpoint == 1) ~ "[3] Stationary at night",
        TRUE ~ RULE
      ),
      behav = case_when(
        (behav == "SResting") & (nightpoint == 1) ~ "SRoosting",
        TRUE ~ behav
      )
    )
  
  logger.trace(paste0("   |> ", sum(data$RULE == "[3] Stationary at night", na.rm = T), " locations re-classified as SRoosting"))
  
  
  
  #### <!> Estimate ACC thresholds at night-time roosting locations -----
  if (ACCclassify == TRUE) {
    roostpoints <- data %>%
      filter(behav == "SRoosting") %>%
      as.data.frame() %>%      
      group_by(ID) %>%
      dplyr::summarise(
        thresx = quantile(var_acc_x, probs = 0.95, na.rm = T) %>% as.vector(),
        thresy = quantile(var_acc_y, probs = 0.95, na.rm = T) %>% as.vector(),
        thresz = quantile(var_acc_z, probs = 0.95, na.rm = T) %>% as.vector()
      )
  }
  
  
  
  
  ### [4] Roosting-site Classification -------------
  
  logger.info("[4] Performing roosting-site classification")
  
  #' Remaining (daytime) resting locations re-classified as roosting if
  #' identified as part of a roosting-site, which is defined as:
  #' 
  #' Consecutive stationary locations (`roostgroup`) encompassing night-time
  #' locations with total overnight distance traveled less than 15 meters
  #' (`roostsite`)
  #' 
  #' NOTE: STravelling locations not affected by this step, even if they were
  #' tagged as part of a roost-site
  
  
  #### [4.1] Identify overnight roosting sites ------
  logger.info(" |- Deriving overnight roosting sites.")
  
  data <- add_roost_cols(data, sunrise_leeway, sunset_leeway)
  
  
  #### [4.2] Apply roosting-site rule ---------
  logger.info(" |- Apply roost-site rule")
  
  data %<>%
    group_by(ID, roostgroup) %>%
    mutate(
      # Reclassify any stationary runs that involve an overnight roost to SRoosting
      RULE = ifelse(!is.na(roostgroup) & any(roostsite == 1) & (behav != "STravelling"), "[4] Stationary at roost site", RULE),
      behav = ifelse(!is.na(roostgroup) & any(roostsite == 1) & (behav != "STravelling"), "SRoosting", behav)
    ) %>%
    ungroup()
  
  # Log results
  logger.info(paste0("   |> ", sum(data$RULE == "[4] Stationary at roost site", na.rm = T), " locations re-classified as SRoosting"))

  
  
  
  ### [5] Non-roosting Stationary Cumulative-time Classification ------------
  
  #' Remaining Resting locations re-classified as Feeding if they are part of a
  #' sequence of non-roosting time-points that remain stationary for an
  #' unusually long period of time
   
  logger.info("[5] Performing non-roosting stationary cumulative-time classification")
  
  #### [5.1] Derive non-roosting stationary runs  -----
  data <- add_nonroost_stationary_cols(data)
  
  #### [5.2] Apply non-roosting stationary Rule  --------- 
  
  #' Re-classify Resting locations assigned with cumulative stationary times
  #' that exceed the 95th percentile of stationary run durations. Percentile
  #' thresholds are individual-based and calculated from the input data
  data %<>% 
    mutate(
      RULE = ifelse(!is.na(cumtimestat) & cumtimestat > dayRunThresh & behav == "SResting", "[5] Extended stationary behaviour", RULE),
      behav = ifelse(!is.na(cumtimestat) & cumtimestat > dayRunThresh & behav == "SResting", "SFeeding", behav),
      #RULE = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "[5] Extended stationary behaviour", RULE),
      #behav = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "SFeeding", behav),
    ) %>%
    ungroup()
  
  # Log results
  logger.trace(paste0("  |> ", sum(data$RULE == "[5] Extended stationary behaviour", na.rm = T), " locations re-classified as SFeeding"))
  
  
  
  
  ### [6] Speed-Time Classification --------------
  
  #' Remaining Resting locations re-classified as Feeding if the speed to next
  #' location is greater the 97.5th percentile of the predicted stationary
  #' speeds at that time of the day (day-hours)
  
  logger.info("[6] Performing speed-given-time classification")
  
  #### [6.1] Fit Stationary Speed Vs day-hours model  ----------------
  logger.info(" |- Deriving thresholds for stationary-speed given hour-since-sunrise.")
  
  progressr::handlers("cli")

  #' setting parallel processing using availableCores() to set # workers.
  #' {future} imports that function from {parallelly}, which  is safe to use in
  #' container environments (e.g. Docker)
  future::plan("multisession", workers = future::availableCores(omit = 2))

  progressr::with_progress({
    # initiate progress signaler
    pb <- progressr::progressor(steps = mt_n_tracks(data))

    data <- data |>
      group_by(ID) |>
      dplyr::group_split() |>
      furrr::future_map(
        .f = ~speed_time_model(
          .x, pb = pb, diag_plots = create_plots, void_non_converging = TRUE
        ),
        .options = furrr_options(
          seed = TRUE,
          packages = c("move2", "sf", "MRSea", "dplyr", "lubridate",
                       "patchwork", "ggplot2")
        )
      ) |>
      mt_stack()
  })

  future::plan("sequential")
  
  # data <- data |>
  #   group_by(ID) |>
  #   dplyr::group_split() |>
  #   purrr::map(
  #     .f = ~speed_time_model(
  #       .x, pb = NULL, diag_plots = create_plots, void_non_converging = TRUE, 
  #       in_parallel = FALSE)
  #   ) |>
  #   mt_stack()
  
  
  #### [6.2] Apply speed-time rule  ----------------
  logger.info(" |- Apply speed-time rule")
  
  data %<>% 
    ungroup() %>%
    mutate(
      RULE = ifelse(!is.na(kmphCI97.5) & !is.na(kmph) & kmph > kmphCI97.5 & behav == "SResting", "[6] Exceed Speed-Time threshold", RULE),
      behav = ifelse(!is.na(kmphCI97.5) & !is.na(kmph) & kmph > kmphCI97.5 & behav == "SResting", "SFeeding", behav)
    )
  
  # Log results
  logger.trace(paste0("  |> ", sum(data$RULE == "[6] Exceed Speed-Time threshold", na.rm = T), " locations re-classified as SFeeding"))
  
  
  
  ### [7] Accelerometer Classification -----
  
  #' Remaining Resting locations re-classified as Feeding if the variance in
  #' acceleration to the next location exceeds the 95th percentile of
  #' acceleration variation values during night-time roosting, in any of the
  #' active accelerometer axis. Percentile thresholds are calculated for each
  #' individual from input data.
  
  if (ACCclassify == TRUE) {
    
    logger.info("[7] Performing accelerometer classification")
    
    data %<>% 
      left_join(roostpoints, by = "ID") %>%
      mutate(
        RULE = case_when(
          # For ACC values that exceed their threshold, reclassify to feeding
          (behav == "SResting") & (var_acc_x > thresx) ~ "[7] ACC not similar to roosting",
          (behav == "SResting") & (var_acc_y > thresy) ~ "[7] ACC not similar to roosting",
          (behav == "SResting") & (var_acc_z > thresz) ~ "[7] ACC not similar to roosting", 
          TRUE ~ RULE
        ),
        behav = case_when(
          # For ACC values that exceed their threshold, reclassify to feeding
          (behav == "SResting") & (var_acc_x > thresx) ~ "SFeeding",
          (behav == "SResting") & (var_acc_y > thresy) ~ "SFeeding",
          (behav == "SResting") & (var_acc_z > thresz) ~ "SFeeding",
          TRUE ~ behav
        )
      ) %>%
      # Move these attributes to track data:
      mt_as_track_attribute(c("thresx", "thresy", "thresz"))
    
    # Log results
    logger.trace(paste0("   ", sum(data$RULE == "[7] ACC not similar to roosting", na.rm = T), " locations re-classified as SFeeding"))
    
  }else{
    logger.warn("[7] Skipping accelerometer classification due to absence of ACC data in all tracks.")
  }
  
  
  
  # Summarise classified behaviour 
  logger.info(" |- Behaviour Classification Summary")
  logger.info(paste0("   |> ", sum(data$behav == "SResting", na.rm = T), " locations classified as SResting"))
  logger.info(paste0("   |> ", sum(data$behav == "STravelling", na.rm = T), " locations classified as STravelling"))
  logger.info(paste0("   |> ", sum(data$behav == "SRoosting", na.rm = T), " locations classified as SRoosting"))
  logger.info(paste0("   |> ", sum(data$behav == "SFeeding", na.rm = T), " locations classified as SFeeding"))
  
  
  
  ## Create plots, if selected ------------------------------------------------------
  
  logger.info("Classification complete. Generating app artifacts")
  if(create_plots == TRUE) {
    
    # create simple plot
    for (id in unique(mt_track_id(data))) {
      
      birddat <- filter_track_data(data, .track_id = id)
      
      birdplot <- birddat |> 
        ggplot(aes(x = sf::st_coordinates(birddat)[, 1], y = sf::st_coordinates(birddat)[, 2]) ) +
        geom_path(col = "gray80") +
        geom_point(aes(colour = behav)) +
        scale_color_brewer(palette = "Set1") +
        labs(
          title = paste0("Behaviour classification for track ID ", id),
          x = "Easting", y = "Northing"
        )
      
      ggsave(
        file = appArtifactPath(paste0("birdtrack_", toString(id), ".png")),
        height = 8,
        width = 10
      )
    }
  }
  
  # Generate summary table
  behavsummary <- table(mt_track_id(data), data$behav)
  write.csv(behavsummary, file = appArtifactPath("behavsummary.csv"))
  
  
  ## Remove nonessential behavioural columns -----------------------------------
  if (keepAllCols == FALSE) {
    logger.trace("Removing all nonessential columns")
    data %<>% dplyr::select(-any_of(
      c(
        "sunrise_timestamp", "sunset_timestamp", "timestamp_local", "ID", "altdiff", 
        "endofday", "endofday_dist_m", "roostsite", "travel01", "cum_trav", "revcumtrav", 
        "roostgroup", "stationaryNotRoost", "stationary_runLts", "cumtimestat", 
        "cumtimestat_pctl", "cumtimestat_pctl_BC",
        "kmphCI2.5", "kmphpreds"
      ) 
    ))
    
  } else {
    # Just get rid of the unuseful columns
    logger.trace("Removing select nonessential columns")
    data %<>% dplyr::select(-any_of(
      c(
        "ID", "endofday_dist_m", "roostsite", "travel01", "cum_trav", "revcumtrav"
      ) 
    ))
  }
  
  # Return final result
  return(data)
  
}



# Helper Functions ====================================================================

#' //////////////////////////////////////////////////////////////////////////////
#' Compute acceleration variance till next location
#' 
acc_var <- function(data, interpolate = FALSE) {
  
  # Store track data for later recall
  tracklevel <- mt_track_data(data)
  tm_col <- mt_time_column(data)
  trk_col <- mt_track_id_column(data)
  
  # Unnest into variances
  data <- data |> 
    dplyr::mutate(
      var = purrr::map(acc_dt, \(acc_events){
        if(!is.null(acc_events$acc_burst)){
          # bind list of matrices (one per ACC event) by row
          all_bursts <- do.call(rbind, acc_events$acc_burst)
          # variance in each active ACC axis
          apply(all_bursts, 2, var)  
        } else{
          NULL
        }
      }, 
      .progress = "Summarising ACC")
    ) %>% 
    tidyr::unnest_wider(var, names_sep = "_") |> 
    # convert back to move2 object (property lost in the last unnest step)
    mt_as_move2(time_column = tm_col, track_id_column = trk_col) |> 
    # re-append track data
    mt_set_track_data(tracklevel)
  
  if (interpolate == TRUE) {
    
    #' Interpolate missing ACCs using the nearest two values on the condition that
    #' both values are within 30mins of the location's timestamp
    
    data <- data |> 
      dplyr::group_by(.data[[trk_col]]) |> 
      dplyr::mutate(
        # add temp columns
        null_acc = purrr::map_lgl(acc_dt, is.null),
        # max of lag to next and previous locations
        acc_max_lag = pmax(
          # time till next location
          difftime(dplyr::lead(.data[[tm_col]]), .data[[tm_col]], units = "mins"),
          # time since previous location
          difftime(.data[[tm_col]], dplyr::lag(.data[[tm_col]]), units = "mins"), 
          na.rm = TRUE)
      ) |> 
      dplyr::mutate(
        # interpolate, keeping leading/trailing NAs and not interpolating more than 4 consecutive NAs
        dplyr::across(dplyr::matches("var_acc_[xyz]"), ~ zoo::na.approx(.x, na.rm = FALSE, maxgap = 4)),
        # nullify interpolated values if lag to previous or next location > 30mins
        dplyr::across(dplyr::matches("var_acc_[xyz]"), ~ ifelse(null_acc & acc_max_lag > 30, NA, .x))
      ) |> 
      # identify locations with interpolated ACC
      dplyr::mutate(interpACC = null_acc & !is.na(var_acc_x)) |> 
      # remove temp columns
      dplyr::select(-null_acc, -acc_max_lag) |> 
      dplyr::ungroup()
  }
  
  return(data)
}

  

#' /////////////////////////////////////////////////////////////////////////////////////////////  
#' Derive and add roosting columns to data 
#' 
#' New columns relevant for classification:
#'  - `roostsite`: identifies overnight roosting sites (based on overnight 
#'  traveled distance < 15m)
#'  - `roostgroup`: identifies groups of locations with roost-like behaviour 
#'  (consecutive non-travelling locations)
#'  
add_roost_cols <- function(data, sunrise_leeway, sunset_leeway){
  
  data %<>% 
    group_by(ID, yearmonthday) %>%
    mutate(
      temptime = lubridate::with_tz(timestamp, lubridate::tz(sunrise_timestamp)), # ensuring all timestamps are in same tz
      # Mark the final and first daytime points in each day 
      endofday = case_when(
        nightpoint == 1 & lag(nightpoint) == 0 ~ "FINAL",
        nightpoint == 1 & lead(nightpoint) == 0 ~ "FIRST", 
        TRUE ~ NA
      ) ) %>% 
    # Calculate time difference from each timestamp to sunrise/sunset (and their leeways):
    mutate(
      sunrise_difference = difftime(temptime, sunrise_timestamp + minutes(sunrise_leeway), units = "mins") %>% abs(),
      sunset_difference = difftime(temptime, sunset_timestamp + minutes(sunset_leeway), units = "mins") %>% abs()
    ) %>%
    mutate(closest = case_when(
      # Mark the closest timestamps to sunrise/sunset, which will proxy for the absence of night points:
      sunrise_difference == min(sunrise_difference, na.rm = T) ~ "SUNRISE",
      sunset_difference == min(sunset_difference, na.rm = T) ~ "SUNSET", 
      TRUE ~ NA
    ))
  
  logger.trace("  |> Identifying locations for overnight roosting checks")
  # Identify which days don't have night points in the morning/at night:
  missing_nightpoints <- data %>%
    as.data.frame() %>%
    group_by(ID, yearmonthday) %>%
    summarise(
      morning = ifelse(any(endofday == "FIRST"), 1, 0),
      evening = ifelse(any(endofday == "FINAL"), 1, 0),
      .groups = "keep"
    )
  
  # Join to data:
  data %<>% left_join(missing_nightpoints, by = c("ID", "yearmonthday"))
  
  # And 'patch' these days up using the sunrise/sunset time-difference proxies:
  data %<>% mutate(
    endofday = case_when(
      # If there is no morning point, but this is the nearest timestamp to sunset, 
      # use it as a proxy:
      is.na(endofday) & is.na(morning) & closest == "SUNRISE" ~ "FIRST",
      # Same applies to evening:
      is.na(endofday) & is.na(evening) & closest == "SUNSET" ~ "FINAL",
      TRUE ~ endofday
    )
  ) %>% dplyr::select(-c("temptime", "morning", "evening", "closest", "sunrise_difference", "sunset_difference"))
  
  # Shortcut for calculating night-distances:
  # Filter dataset to only the marked final/first point
  # Bind distance using mt_distance and keep only overnight distances
  # then merge back into main dataset
  logger.trace("  |> Generating overnight roosting distances")
  nightdists <- data %>% 
    filter(!is.na(endofday)) %>% 
    ungroup() %>%
    mutate(
      endofday_dist_m = mt_distance(., units = "m"),
      endofday_dist_m = ifelse(endofday == "FINAL", endofday_dist_m, NA)
    ) %>%
    as.data.frame() %>%
    dplyr::select(c(ID, mt_time_column(.), endofday_dist_m))
  
  data %<>% left_join(nightdists, by = c("ID", mt_time_column(.)))
  
  # This gives us one overnight-distance measure at the end of each bird's day
  data %<>% mutate(
    roostsite = ifelse(
      !is.na(endofday_dist_m) & endofday_dist_m < 15,
      1, 0
    )
  ) 
  
  logger.trace("  |> Generating roost-group data")
  #  Calculate cumulative travel and reverse cumulative travel per day
  data %<>%
    group_by(ID, yearmonthday) %>%
    mutate(travel01 = ifelse(stationary == 1, 0, 1)) %>%
    mutate(cum_trav = cumsum(travel01),
           revcum_trav = spatstat.utils::revcumsum(travel01)) %>%
    ungroup() %>%
    mutate(
      # Generate runs of stationary behaviour before/after final/first location:
      roostgroup = ifelse(cum_trav == 0 | revcum_trav == 0, 1, 0),
      roostgroup = data.table::rleid(roostgroup),
      roostgroup = ifelse(cum_trav != 0 & revcum_trav != 0, NA, roostgroup)
    ) 
}



#' /////////////////////////////////////////////////////////////////////////////////////////////  
#' Derive columns required for the non-roosting stationary cumulative time  
#' 
#' Relevant added columns 
#'  - `cumtimestat`: cumulative time spent, up to each location, in a run of
#'  non-roosting stationary time-points. 0's attributed to locations that are
#'  not part of a stationary run#'  
#'  - `dayRunThresh`: 95th percentile of stationary run durations, per bird  
add_nonroost_stationary_cols <- function(data){
  
  # Generate non-roosting stationary run-length data
  data %<>%
    group_by(ID) %>%
    mutate(
      stationaryNotRoost = ifelse(stationary == 1 & behav %!in% c("SRoosting"), 1, 0),
      # Adding condition to break runs spreading over large time gaps in GPS
      # transmission, in order to stop inflation of run durations in `cumtimestat`.
      # For now, hard-coding boundary to 3/4 of 24hrs has a value greater than 
      # regular and acceptable overnight transmission gaps seen in some studies
      stationaryNotRoost = ifelse(stationaryNotRoost == 1 & timediff_hrs > 16, NA, stationaryNotRoost),
      stationary_runLts = data.table::rleid(stationaryNotRoost == 1),     # id runs of stationary & non-stationary entries
      stationary_runLts = ifelse(stationaryNotRoost == 0, NA, stationary_runLts)
    ) %>%
    group_by(ID, stationary_runLts) %>%
    mutate(
      cumtimestat = cumsum(as.numeric(timediff_hrs)), # compute cumulative time (hrs) spent stationary & non-stationary
      cumtimestat = ifelse(stationaryNotRoost == 0 | cumtimestat < 0, 0, cumtimestat)
    ) %>%
    group_by(ID) %>%
    mutate(
      # cumtimestat_pctl = 1 - (match(cumtimestat, sort(cumtimestat))/(length(which(cumtimestat!="NA")) + 1)), # From original code, which perhaps is not doing what's suppposed to do
      cumtimestat_pctl = 1 - ecdf(cumtimestat)(cumtimestat) 
    )
  
  
  # find the duration of every stationary run
  eventtimes <- data %>% data.frame() %>%
    group_by(ID, stationary_runLts) %>%
    summarise(
      runtime = suppressWarnings(max(cumtimestat, na.rm = TRUE)),
      runtime = ifelse(is.infinite(runtime), 0, runtime), 
      .groups = "drop"
    ) %>%
    group_by(ID) %>% 
    mutate(dayRunThresh = quantile(runtime, probs = 0.95))
  
  # add run time back to main data
  data %<>% left_join(., eventtimes, by = c("ID", "stationary_runLts"))
  
  data
}



#' /////////////////////////////////////////////////////////////////////////////////////////////
#' Fit stationary-speed given decimal day-hours, for one single track
#' 
#' @param dt a move2 object for one single track
#' @param pb a Progressor Function generated via `progressr::progressor` to
#'   signal updates
#' @param diag_plots logical, whether to generate model diagnostic plots and
#'   export them as App artifacts
#' @param in_parallel logical, whether the function is being called inside a
#'   parallel worker. This is required for managing sink connections
#' @param model_obj logical, whether to return the fitted model object.
#' 
#' @return  If `model_obj = TRUE`, a list with: (i) the input data with 3 extra
#'   columns for the predicted values and 95% CIs and (ii) the fitted model
#'   object. Otherwise, only the input data with model predictions.
speed_time_model <- function(dt, 
                             pb = NULL, 
                             diag_plots = TRUE, 
                             in_parallel = TRUE, 
                             void_non_converging = TRUE,
                             model_obj = FALSE
                             ){
  
  id <- mt_track_id(dt) |> unique() |> as.character()
  
  if(length(id) > 1){
    stop("`dt` contains data for more than one track. Please provide a move2 object with a single track")
  } 
  
  #logger.info(paste0("   |> Fitting model for track ", id, " @ ", lubridate::now()))
  logger.info(paste0("   |> Fitting model for track ", id))
  
  # Check number of days covered in dataset
  n_days <- round(difftime(max(dt$timestamp), min(dt$timestamp), units = "day"), 1)
  
  #' Impose condition where fitting only performed if there is more than 10 days
  #' of data, otherwise data deemed insufficient to robustly describe the
  #' relationship between stationary speeds and time-of-the-day (expressed as
  #' hours-since-sunrise)
  if(n_days < 10){
    logger.warn(
      paste0(
        "      |x Track data covers < 10 days. This is deemed insufficient to model speed-give-time robustly.\n", 
        "             |x Speed-time classification will not be applied to this track."
      ))
    
    fit <- NULL
    
  } else {
    
    #' Add disclaimer for tracks covering more than 5 days from the modelled
    #' last 30 days, which will comprise events outside the scope of the fitted
    #' model (i.e. the fitted speed-given-time relationship may not hold).
    #' Going forward, we can extend to 30day-period models
    #' (i.e. 0-30days, 30-60days, ...), so that min 10 days requirement can be
    #' employed? Or alternatively, add a 30day-period term to the model?
    if(n_days > 35){
      logger.warn(
        paste0(
          "      |! Track data covers ", n_days, " days, whereas current modelling is constrained to the last 30 days in the data.\n", 
          "             |! Speed-time model still being fitted but BEWARE: predictions may be flawed on locations ealier than 30 days."
        ))
    }
    
    newdat <- dt %>%
      dplyr::mutate(
        month = month(timestamp),
        response = kmph + 0.00001
      ) %>%
      dplyr::filter(
        !is.na(response),
        stationary == 1,
        timestamp > (max(timestamp) - days(30))
      )
    
    
    initialModel <- suppressWarnings(
      glm(response  ~ 1 , family = Gamma(link="log"), data = newdat)
    )
    
    salsa1dlist <- list(
      fitnessMeasure = 'BIC',
      minKnots_1d = c(1),
      maxKnots_1d = c(5),
      startKnots_1d = c(1),
      degree = c(2),
      maxIterations = 10,
      gaps = c(0),
      splines = c("ns")
    )
    
    # run SALSA
    non_conv_warn <- FALSE
    fit <- rlang::try_fetch(

      suppressPackageStartupMessages( # prevent dependency loading msgs on workers' launch

        runSALSA1D(
          initialModel,
          salsa1dlist,
          varlist=c("hrs_since_sunrise"),
          splineParams=NULL,
          datain=newdat,
          predictionData = filter(dt, !is.na(kmph)),
          panelid = newdat$yearmonthday,
          suppress.printout = TRUE)$bestModel
      ),

      error = \(cnd){
        # needed to handle unclosed connection in some error cases of runSALSA1D
        if(conditionMessage(cnd) == "NA/NaN/Inf in 'x'") sink()
        logger.warn(
          paste0(
            "      |x Ouch!! Something went wrong while fitting the model.\n",
            "             |x `runSALSA1D()` returned the following error message:\n",
            "             |x \"", conditionMessage(cnd), "\"\n",
            "             |x Speed-time classification will not be applied to this track."
          ))
        return(NULL)
      },

      # In addition, muffle warnings related with non-converging glm fits, which
      # are dealt with next
      warning = \(cnd){
        if(conditionMessage(cnd) == "glm.fit: algorithm did not converge"){
          non_conv_warn <<- TRUE
          rlang::cnd_muffle(cnd)
        }
        rlang::zap()
      }
    )
    
    # Handling non-converging warnings in model fitting. If sense check on 
    # model term p-values fails (i.e. any nonsensical pvalues of < 1e-100),
    # invalidate speed-time classification (when option `void_non_converging` is TRUE)
    if(not_null(fit) & non_conv_warn == TRUE & void_non_converging == TRUE){
      #browser()
      fit_coeffs <- as.data.frame(summary(fit)$coefficients)
      #bad_fit <- any(fit_coeffs[, "Std. Error"] > abs(fit_coeffs[, "Estimate"]))
      bad_fit <- any(fit_coeffs[["Pr(>|t|)"]] < 1e-100)
      if(bad_fit){
        logger.warn(
                  paste0(
                    "      |x Aargh!! Convergence issues found during model fitting.\n",
                    "             |x Speed-time classification will not be applied to this track."
                  ))
        fit <- NULL
      }
    }
    
  }
  
  
  if(not_null(fit)){
    
    # NOTE: Predicting to full dataset for convenience in data wrangling - i.e. no 
    # post-processing required to combine predictions for stationary-only events 
    # with the full data). No apparent cost in terms of computational speed
    # Non-stationary events will be ignored in the subsequent classification step
    
    dt$kmphpreds <- predict(object = fit, newdata = dt) |> as.vector()
    
    boots <- suppressPackageStartupMessages( # prevent dependency loading msgs on workers' launch
      MRSea::do.bootstrap.cress.robust(
        model.obj = fit,
        predictionGrid = dt,
        B = 1000, robust = TRUE,
        cat.message = FALSE)
    )
    
    cis <- MRSea::makeBootCIs(boots)
    
    # # -- Construct prediction intervals
    # d = summary(fit)$dispersion
    # predint <- apply(boots, 2, function(x){rgamma(n = length(x), shape = 1/d, scale= x*d)})
    # pis <- t(apply(predint, 1, FUN = quantile,probs = c(0.025, 0.975)))
    
    dt <- dt %>%
      mutate(
        `kmphCI2.5`= cis[,1],
        `kmphCI97.5` = cis[,2],
        #"kmphPI2.5" = pis[,1], "kmphPI97.5" = pis[,2]
      )
    
    # build diagnostic plots and export as artifacts
    if(diag_plots){
      
      p_fit <- plot_model_fit(dt, fit)
      p_acf <- MRSea::runACF(newdat$yearmonthday, fit, suppress.printout = TRUE, printplot = FALSE)
      #p_acf <- plot_acf(fit)
      p_resids <- plot_diagnostics(fit, plotting = "r", print = FALSE)
      p_obs_fit <- plot_diagnostics(fit, plotting = "f", print = FALSE)
      p_mn_var <- plotMeanVar(fit, print = FALSE, cut.bins = find_cut.bins(fit))
      
      # Next graph involves model updating to more flexible predictor, so
      # refitting brings new issues at times. Handling errors by skipping the plotting.
      p_cmltv_rsd <- tryCatch(
        plot_cmltv_resids(fit, varlist = "hrs_since_sunrise", variableonly = TRUE, print = FALSE),
        error = \(cnd) grid::textGrob('Cumulative Residuals Plot Not Available')
      )
      
      
      # p_cmltv_rsd <- rlang::try_fetch(
      #   plot_cmltv_resids(fit, varlist = "hrs_since_sunrise", variableonly = TRUE, print = FALSE),
      #   error = \(cnd){
      #     grid::textGrob('Cumulative Residuals Plot Not Available')
      #   },
      #   warning = \(cnd){
      #     if(conditionMessage(cnd) == "glm.fit: algorithm did converge"){
      #       rlang::cnd_muffle(cnd)
      #       rlang::zap()
      #     }else{
      #       warning(conditionMessage(cnd), call. = FALSE)
      #     }
      #   }
      # )
      
      p_diags <- (p_fit + p_resids) / (p_acf + p_obs_fit) / (p_mn_var + p_cmltv_rsd) + 
        patchwork::plot_annotation(title = paste0("Track ID: ", id), tag_levels = 'A') &
        theme_bw() & 
        theme(
          legend.position = "top", 
          legend.title = element_blank(),
          plot.title = element_text(size = 10),
          plot.tag = element_text(size = 9)
        )
      
      
      ggplot2::ggsave(
        filename = appArtifactPath(paste0("speed_hrs_diagnostics - ", id, ".png")),
        plot = p_diags, 
        height = 10, width = 11
      )
    }
    
  } else{
    
    dt <- dt |>
      mutate(
        kmphpreds = NA,
        `kmphCI2.5` = NA,
        `kmphCI97.5` = NA,
        #`kmphPI2.5` = NA, `kmphPI97.5` = NA
      )
  }
  
  # Update progress bar, if active
  if(not_null(pb)){
    pb()  
  }
  
  
  if(model_obj){
    return(list(dt = dt, fit = fit))
  }else{
    return(dt)  
  }
  
}




#' /////////////////////////////////////////////////////////////////////////////////////////////
plot_model_fit <- function(dt, fit){
  
  dt |> 
    as_tibble() |> 
    distinct(hrs_since_sunrise, .keep_all = TRUE) |> 
    mutate(
      ci_lbl = "95% Confidence Interval",
      fitted_lbl = "Expected Value"
    ) |> 
    ggplot(aes(x = hrs_since_sunrise)) +
    geom_ribbon(aes(ymin = kmphCI2.5, ymax = kmphCI97.5, fill = ci_lbl), alpha = 0.5) +
    geom_line(aes(y = kmphpreds, col = fitted_lbl), linewidth = 1) +
    geom_rug(sides = "b") +
    # add stationary points with speeds above the upper boundary of the 95% CI
    geom_point(
      data = dt |> filter(kmph > kmphCI97.5, stationary == 1),
      aes(y = kmph),
      col= "red", alpha = 1/4, size = 1
    ) +
    scale_fill_manual(values = "#90CAF9") +
    scale_colour_manual(values = "black") +
    # add selected knots in fitted model
    geom_vline(
      xintercept = fit$splineParams[[2]]$knots, colour = "gray20", 
      linetype = "dashed", linewidth = 0.5)
} 


#' #' /////////////////////////////////////////////////////////////////////////////////////////////
#' # Adapted from https://stackoverflow.com/questions/17788859/acf-plot-with-ggplot2-setting-width-of-geom-bar
#' plot_acf <- function(fit, alpha = 0.05){
#'   
#'   pears_resids <- residuals(fit, type="pearson")
#'   acf_out <- acf(pears_resids, plot = FALSE)
#'   acf_dt <- with(acf_out, tibble(lag, acf))
#'   
#'   # CI for alpha
#'   lim1 <- qnorm((1 + (1 - alpha))/2)/sqrt(acf_out$n.used)
#'   lim0 <- -lim1
#'   
#'   ggplot(data = acf_dt, aes(x = lag, y = acf)) +
#'     geom_hline(aes(yintercept = 0)) +
#'     geom_segment(aes(xend = lag, yend = 0)) +
#'     labs(
#'       y = "Autocorrelation in Pearson Residuals", 
#'       #y = "ACF"
#'     ) +
#'     geom_hline(aes(yintercept = lim1), linetype = 2, color = 'blue') +
#'     geom_hline(aes(yintercept = lim0), linetype = 2, color = 'blue')
#'   
#' }
#' 



#' /////////////////////////////////////////////////////////////////////////////////////////////
#'  Hacked from MRSea::runDiagnostics() to offer the option of returning plot objects (`print`)
#'  
plot_diagnostics <-function(model, plotting='b', save=FALSE, print = TRUE, label = NULL){
  
  p_theme <- theme_bw() +
    theme(
      panel.grid.major=element_blank(), 
      axis.text.x=element_text(size=10), 
      axis.text.y=element_text(size=10), 
      axis.title.x=element_text(size=12), 
      axis.title.y=element_text(size=12)
    )
  
  df <- data.frame(
    fits = fitted(model),
    response = model$y)
  
  # Set printing action
  if(print & plotting =='b'){
    devAskNewPage(ask=TRUE)
  }
  
  if(plotting =='b' | plotting=='f'){
    
    #Assessing predictive power
    #r-squared:
    r2 <- 1-(sum((df$response - df$fits)**2)/sum((df$response - mean(df$response))**2))
    
    #concordance correlation
    num <- 2*sum((df$response - mean(df$response))*(df$fits - mean(fitted(model))))
    den <- sum((df$response - mean(df$response))**2) + sum((df$fits - mean(df$fits))**2)
    rc <-num/den
    
    f <- ggplot(df) + 
      geom_point(aes(response, fits), alpha=0.15) + 
      geom_abline(intercept=0, slope=1) + 
      labs(
        x='Observed Values', 
        y='Fitted Values', 
        title=paste("Concordance correlation: ", 
                       round(rc,4), "\nMarginal R-squared value: ", 
                       round(r2,4), sep="")
      ) +
      p_theme
    
    if(save) ggsave(paste0(label, "FitPlots_fitted.png"), f, height=6, width=8)
    if(print) plot(f)
  }
  
  if(plotting =='b' | plotting=='r'){
    
    scaledRes <- residuals(model, type="response")/
      sqrt(family(model)$variance(fitted(model))*as.numeric(summary(model)$dispersion[1]))
    
    sm <- lowess(fitted(model), scaledRes)
    
    df <- df |> 
      dplyr::mutate(
        res=scaledRes, 
        smx=sm$x, 
        smy=sm$y
      )
    
    r <- ggplot(df) + 
      geom_point(aes(fits, res), alpha=0.15)+ 
      geom_line(aes(smx, smy), col='red') + 
      geom_abline(intercept=0, slope=0) + 
      labs(x='Fitted Values', y='Scaled Pearsons Residuals') +
      p_theme
    
    if(save) fgsave(paste0(label, "FitPlots_resids.png"), r, height=6, width=8)
    if(print) plot(r)
  }
  
  if(!print){
    if(plotting=='b'){
      return(list(obs_vs_fitted = f, scaled_resids = r))
    } else if(plotting=='f'){
      return(f)
    } else if(plotting=='r'){
      return(r)
    }
  }else{
    devAskNewPage(ask=FALSE)
    return(invisible())
  }
}


#' /////////////////////////////////////////////////////////////////////////////////////////////
#'  Hacked from MRSea::plotCumRes() to add option of returning plot objects (`print`).
#'  Required replacing loop with imap() to deal with scoping issues with using
#'  loops and ggplots
plot_cmltv_resids <- function(model, varlist = NULL, print = TRUE, save = FALSE, 
                              label = "", variableonly = FALSE){
  
  require(splines)
  
  if (is.null(varlist)) {
    namesOfx <- c(c("Predicted", "Index"))
  }
  else {
    namesOfx <- c(varlist, c("Predicted", "Index"))
  }
  
  md_cls <- class(model)[1]
  if (md_cls %in% c("geeglm", "glm", "gamMRSea")) dat <- data.frame(model$data)
  if (md_cls == "gam")  dat <- data.frame(model$model)
  
  if (!is.null(varlist)) {
    coefpos <- c()
    for (z in 1:(length(namesOfx) - 2)) {
      coefpos <- c(coefpos, grep(namesOfx[z], names(dat)))
    }
  }
  
  dat <- dat %>% mutate(Predicted = fitted(model), Index = 1:n())
  
  if (variableonly) plotvar <- varlist else plotvar <- namesOfx
  
  p_out <- purrr::imap(plotvar, \(varname, z){
    
    type = "response"; yl <- "Response Residuals"
    
    if (varname == "Predicted") {
      type = "response"
      yl = "Response Residuals"
    }
    
    plotdat <- dat %>% mutate(m.resids = residuals(model, type = type), resids.lbl = "resids")
    plotdat <- eval(parse(text = paste0("arrange(plotdat, ", varname, ")")))
    plotdat <- plotdat %>% mutate(m.cumsum = cumsum(m.resids), cumsum.lbl = "cmltv_rsd")
    
    if (z < (length(namesOfx) - 1)) {
      covardat <- dat[model$y > 0, coefpos[z]]
      newknots <- as.vector(quantile(covardat, seq(0.05, 0.95, length = 7)))
      newknots <- unique(newknots)
      term <- labels(terms(model))[grep(namesOfx[z], labels(terms(model)))]
      newterm <- paste("bs(", namesOfx[z], ", knots= c(",
                       paste(newknots, sep = " ", collapse = ","), "))",
                       sep = "")
      eval(parse(text = paste("covarModelUpdate<-update(model, .~. -",
                              term, " +", newterm, ", data=plotdat)", sep = "")))
      plotdat <- plotdat %>% 
        mutate(
          new.fits = fitted(covarModelUpdate),
          new.resids = residuals(covarModelUpdate, type = type),
          new.cumsum = cumsum(new.resids),
          new.cumsum.lbl = "cmltv_rsd_flex"
        )
    }
    
    maxy <- max(plotdat$m.resids, plotdat$m.cumsum)
    miny <- min(plotdat$m.resids, plotdat$m.cumsum)
    
    plt <- ggplot(plotdat) +
      geom_point(aes(x = pull(plotdat, varname), y = m.resids, colour = resids.lbl)) +
      xlab(varname) +
      ylab(yl) +
      geom_line(aes(x = pull(plotdat, varname), y = m.cumsum, colour = cumsum.lbl)) +
      geom_hline(yintercept = 0) +
      labs(x = varname, y = yl) +
      theme_bw()
    
    if (z < (length(namesOfx) - 1)) {
      plt <- plt +
        geom_line(
          aes(x = pull(plotdat, varname), y = new.cumsum, colour = new.cumsum.lbl)) +
        geom_point(
          aes(x = pull(plotdat, varname), y = new.resids, colour = new.cumsum.lbl),
          alpha = 1/5)
    }
    plt +
      scale_colour_manual(
        values = c(resids ="turquoise4", cmltv_rsd = "black", cmltv_rsd_flex = "grey"),
        breaks= c("resids", "cmltv_rsd", "cmltv_rsd_flex"),
        name = "",
        labels = c("Residuals", "Cumulative\n Residuals", "Cumulative Residuals under\nhigher model flexibility")
      ) +
      theme(legend.position="top")
  })
  
  if(save){
    iwalk(p_out, 
          ~ggsave(filename = paste("CumRes_", namesOfx[.y], label, ".png", sep = ""),
                  plot = .x, height = 600, width = 700, units = "px"))
  }
  
  if(print){
    devAskNewPage(ask = TRUE)
    print(p_out)  
    devAskNewPage(ask = FALSE)
    invisible()
  }else{
    if(length(p_out) == 1){
      p_out <- p_out[[1]]
    } else{
      names(p_out) <- plotvar
    }
    return(p_out)
  }
  
}



#' /////////////////////////////////////////////////////////////////////////////////////////////
#' Helper function to find the a suitable value for parameter `cut.bins` for
#' function MRSea::plotMeanVar().
#' 
#' This is required to automate the generation of that plot, i.e. in a
#' non-interactive session such as MoveApps
find_cut.bins <- function(model){
  
  cut.bins <- 30
  nainthahouse <- TRUE
  
  while(nainthahouse){
    cutpts <- unique(quantile(fitted(model), prob = seq(0, 1, length = cut.bins)))
    mycuts <- cut(fitted(model), breaks = cutpts)
    meanfits <- tapply(fitted(model), mycuts, mean)
    nainthahouse <- any(is.na(meanfits))
    if(nainthahouse) cut.bins <- cut.bins - 1
  }
  
  cut.bins
}

