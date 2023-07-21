library('move2')
library('lubridate')
library('magrittr')
library('dplyr')
library('ggplot2')
`%!in%` <- Negate(`%in%`)

rFunction = function(data, rooststart, roostend, travelcut,
                     create_plots = TRUE,
                     use_sunrise = FALSE,
                     sunrise_leeway = 0,
                     sunset_leeway = 0 #, 
                     # second_stage_model = NULL, fit_speed_time  # Will be reincorporated later
                     ) {
  
  
  # Prepare Second-Stage Models --------------------------------------------------------------------------------------------

      #' This section will be re-incorporated at a later date when the MoveApp required to
      #' generate the speed-time models is published. For now, this stage of classification
      #' will be skipped
    
  # Call model file, if provided:
  # modelPath <- paste0(getAppFilePath("second_stage_model", fallbackToProvidedFiles = TRUE), "modelfit.rds")
  # providedModels <- readRDS(modelPath)
  # 
  # if(is.character(providedModels)) {
  #   nomodels <- TRUE
  #   logger.warn(providedModels)
  # } else {
  #   nomodels <- FALSE
  #   logger.info(paste0("Models provided for tags ", toString(names(providedModels)), ". Proceeding with classification"))
  # }
  # 
  # # Call standard classification model file, for all IDs not included in the above file:
  # standardModelPath <- paste0(getAppFilePath("standard_model", fallbackToProvidedFiles = TRUE), "standard_model.rds")
  # standardModel <- readRDS(standardModelPath)
  # 
  # 
  
  # Check Input Data ----------------------------------------------------------------------------------------------
  
  logger.trace(paste0(
    "Input data provided:  \n", 
    "rooststart: ", toString(rooststart), "\n", 
    "roostend: ", toString(roostend), "\n",
    "travelcut: ", toString(travelcut), "\n",
    "sunrise_leeway: ", toString(sunrise_leeway), "\n", 
    "sunset_leeway: ", toString(sunset_leeway), "\n",
    "input data dimensions: ", toString(dim(data))
  ))

  if(nrow(data) == 0) {
    logger.warn("Input data is empty. Returning input.")
    return(data)}
  
  if(!all(between(c(rooststart, roostend), 0, 24))) {
    logger.fatal("Start or end of roosting hours is not a valid hour. Terminating workflow - please use valid settings")
    stop("Start or end of roosting hours is not a valid hour. Terminating workflow - please use valid settings")
  }
  
  if(rooststart < roostend) {
    logger.warn("This MoveApp is not yet optimised for nocturnal species. Classifications will be inaccurate and errors may be thrown")
  }
  
  if(travelcut <=  0 | !is.numeric(travelcut)) {
    logger.fatal("Speed cut-off for travelling behavour is not a valid speed. Returning input - please use valid settings")
    stop("Speed cut-off for travelling behavour is not a valid speed. Returning input - please use valid settings")
  }
  
  if (use_sunrise == TRUE & "sunrise_timestamp" %!in% colnames(data)) {
    logger.warn("'sunrise_timestamp' is not a column in the input data. Sunrise-sunset classification cannot be performed. Defaulting to manually provided roosting hours - please use the 'Add Local and Solar Time' MoveApp in this workflow before this App")
    use_sunrise <- FALSE
  }
  if (use_sunrise == TRUE & "sunset_timestamp" %!in% colnames(data)) {
    logger.warn("'sunset_timestamp' is not a column in the input data. Sunrise-sunset classification cannot be performed. Defaulting to manually provided roosting hours - please use the 'Add Local and Solar Time' MoveApp in this workflow before this App")
    use_sunrise <- FALSE
  }
  
  if(is.null(sunrise_leeway)) {
    logger.warn("No sunrise leeway provided as input. Defaulting to no leeway")
    sunrise_leeway <- 0
  }
  if(is.null(sunset_leeway)) {
    logger.warn("No sunset leeway provided as input. Defaulting to no leeway")
    sunset_leeway <- 0
  }
  
  logger.trace("Input is in correct format. Proceeding with first-stage classification for all IDs")
  
  
  # First-Stage Classification --------------------------------------------------------------------------------------
  
  needcols <- c("hourmin", "yearmonthday")
  if(!all(needcols %in% colnames(data))) {
    data %<>% dplyr::mutate(
      hourmin = hour(mt_time(.)) + minute(mt_time(.))/60 + second(mt_time(.))/3600,
      yearmonthday = stringr::str_replace_all(stringr::str_sub(lubridate::date(mt_time(data)), 1, 10), "-", "")
    )
  }
  
  # Generate necessary data
  data$dist_m <- as.vector(move2::mt_distance(data))
  data$kmph <- move2::mt_speed(data) %>% units::set_units("km/h") %>% as.vector()
  #data$timediff_hrs <- lubridate::mt_time_lags(data)
  data %<>% 
    arrange(mt_track_id(data), mt_time(data)) %>%
    # distinct(timestamp, .keep_all = TRUE) %>%
    mutate(
      #dist_m = sqrt((x - lag(x, default = NA))^2 + (y - lag(y, default = NA))^2),
      timediff_hrs =   as.numeric(difftime(mt_time(data), lag(mt_time(data), default = mt_time(data)[1]), units = "hours")), 
      #kmph = move2::mt_speed(data),
      hour = lubridate::hour(mt_time(data))
    ) 
  
  
  ## Speed Classification -----------------------------------------------------
  
     #' Speed Classification
     #' Anything above 3km/h is STravelling
     #' Anything below is initially SResting

  data %<>%
    mutate(
      behav = case_when(
        kmph < travelcut  ~ "SResting",
        kmph > travelcut  ~ "STravelling",
        TRUE ~ "Unknown"
      ),
      stationary = ifelse(behav=="STravelling", 0, 1)
    )
  logger.info("Speed classification complete")
  
  
  ## Altitude Reclassification -------------------------------------------------
  
      #' If a bird is ascending, it is STravelling
      #' If a bird is flatlining, it remains SResting
      #' If a bird is descending, 
      #'      Next location is ascending/descending ==> STravelling
      #'      Next location is flatlining ==> remains SResting
  
  
  if ("altitude" %in% colnames(data)) {
    logger.info("Altitude column identified. Beginning altitude classification")

    # Classify altitude changes
    data %<>% mutate(altdiff = altitude - lag(altitude),
                     altchange = case_when(
                       altdiff < 25 ~ "descent",
                       altdiff > 25 ~ "ascent",
                       TRUE ~ "flatline"
                     ))
    data %<>%
      mutate(behav = case_when(
        behav == "SResting" & altchange == "ascent" ~ "STravelling",
        behav == "SResting" & altchange == "descent" & lead(altchange) %in% c("descent", "ascent") ~ "STravelling",
        TRUE ~ behav
      ))
    
  } else {
    logger.warn("No column named 'altitude' present. Skipping altitude classification")
  }
  
  
  # Day-Night Reclassification --------------------------------------------------
  
      #' If a bird is SResting and it is night time, the bird is SRoosting
      #' Otherwise, remains unchanged
  
  if (use_sunrise == TRUE) {
    data %<>%
      mutate(behav = case_when(
        behav == "SResting" & !between(mt_time(.), sunrise_timestamp + lubridate::minutes(sunrise_leeway), sunset_timestamp + lubridate::minutes(sunset_leeway))
      ))
  } else {
    data %<>%
      mutate(behav = case_when(
        behav == "SResting" & !between(hour(mt_time(.)), roostend, rooststart) ~ "SRoosting",
        TRUE ~ behav
      ))
  }

  
  # Second-Stage Classification ----------------------------------------------------------------------------------
  
  ## Accelerometer Data -----------------------------------------------------------------------------------------------
  
        # Accelerometer operations will go here
        # Will be implemented later
        #
  
  
  # We fit the second-stage model for each animal, and then re-stack the data into a move2 object
  newdat <- list()
  
  
  for (tag in unique(mt_track_id(data))) {
    
    # If there is not enough data for this bird, skip its second-stage
    birddat <- filter_track_data(data, .track_id = tag)
    if(nrow(birddat) < 10) {
      logger.info(paste0("Insufficient data exists for ID ", tag, " with associated model. Skipping second-stage classification"))
      next}
    
    logger.info(paste0("Beginning second-stage classification for ID ", tag))
    
    
    
    ## Cumulative Stationary Time Reassignment ===============================================================
    
    #' ~~~ Calculate cumulative time spent stationary
    birddat %<>%
      mutate(
        stationary_runLts = data.table::rleid(stationary == 1)     # id runs of stationary & non-stationary entries
      ) %>%
      group_by(stationary_runLts) %>%
      mutate(
        cumtimestat = cumsum(as.numeric(timediff_hrs)), # compute cumulative time (hrs) spent stationary & non-stationary
        cumtimestat = ifelse(stationary == 0 | cumtimestat < 0, 0, cumtimestat)
      ) %>%
      ungroup() %>%
      mutate(
        cumtimestat_pctl = 1 - (match(cumtimestat, sort(cumtimestat))/(length(which(cumtimestat!="NA")) + 1)), # From original code, which perhaps is not doing what's suppposed to do
        cumtimestat_pctl_BC = 1 - ecdf(cumtimestat)(cumtimestat)                                               # Correct calculation?
      )
    
    # Reasssign SResting => SFeeding
    # The longest 5 percentiles of runs of stationary behaviours will be reclassified as SFeeding
    
    birddat %<>% 
      mutate(
        behav = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "SFeeding", behav)
      ) 
    
    


    
    ## Speed-Time Model Reassignment ================================================================================

        #' This section will be reincorporated at a later date and allow reclassification
        #' based on a speed-time model fitted to each animal using an alternative MoveApp.
        #' For now, feeding will only be classified by cumulative stationary time
        
    # Force-stop this section until incorporated:
    fit_speed_time <- FALSE
    
    # Only perform this if the option is selected:
    if (fit_speed_time == TRUE) {
      
      
      ### Calling model ==========================================================================================
      
      # Retrieve model
      if (tag %in% names(providedModels) & nomodels == FALSE) {
        # If animal has an associated model, call it:
        fit <- providedModels[[tag]]
      } else {
        # If no model, use the fallback model:
        logger.warn(paste0("No model has been fitted to ID ", tag, " . Applying fallback model for Gyps Africanus vultures"))
        fit <- standardModel
      }
      
      
      ### Perform reclassification ===========================================================================
      
      x_values <- sort(unique(birddat$hourmin))
      b <- cbind(rep(1, length(x_values)), bs(x_values, 
                                              knots=fit$splineParams[[2]]$knots,
                                              Boundary.knots = fit$splineParams[[2]]$bd,
                                              degree=2))
      realfit <- exp(b%*%coef(fit))
      rcoefs<- rmvnorm(1000, coef(fit), summary(fit)$cov.unscaled)
      try(rcoefs<- rmvnorm(1000, coef(fit), summary(fit)$cov.robust), silent = TRUE)
      reta <- (b%*%t(rcoefs))
      rpreds <- exp(reta)
      #rpreds<- exp(reta)/(1+exp(reta))
      quant.func <- function(x){quantile(x, probs=c(0.025, 0.975), na.rm=T)}
      cis <- t(apply(rpreds, 1, quant.func))
      
      # -- Construct prediction intervals
      predint <- apply(rpreds, 2, function(x){rpois(length(x), x)})
      pis <- t(apply(predint, 1, quant.func))
      
      # -- Calculate empirical p-values
      empPval <- c()
      
      for(i in 1:nrow(birddat)){  # i = 271
        
        # BC: Reckon there is a bug in the following line of code, as it can produce a vector of length > 1 in the
        #     right-hand side of the condition in some occasions (duplicated hour-day entries, with different speeds),
        #     leading to pairwise comparisons with the vector on the left-hand side. Don't think this was the 
        #     intention in the first place. The amended code simply compares the predicted draws against one speed value at time
        #     
        empPval[i]<- length(which(predint[ which(x_values==birddat$hourmin[i]), ] < birddat$kmph[birddat$hourmin==birddat$hourmin[i] & birddat$yearmonthday==birddat$yearmonthday[i]]))/1000
      }
      
      
      birddat %<>% 
        mutate(
          empPval = empPval,
          behav = ifelse(empPval < 0.05 & behav == "SResting", "SFeeding", behav),
        ) 
      
    }
    
    # The following is definitely not the best way to re-join the objects
    # But we're going with it for now
    newdat[[tag]] <- birddat
    logger.info(paste0("Second-stage classification complete for ID ", tag))
    
  }
  
  
  # Return data to move2 object -----------------------------------------------------------------------------------------------
  
  # Rejoin move2 objects into classified data:
  logger.info("All second-stage classifications complete. Re-stacking to move2 object")
  updateddata <- move2::mt_stack(newdat) 
  
  
  
  # Create plots, if selected ------------------------------------------------------
  if(create_plots == TRUE) {
    
    # create simple plot
    for (bird in unique(mt_track_id(data))) {
      
      # Create artefact
      png(appArtifactPath(
        paste0("birdtrack_", toString(bird), ".png")
      ))
      
      birddat <- data %>% 
        filter_track_data(
          .track_id = bird
        )
      
      birdplot <- ggplot(data = birddat, aes(x = sf::st_coordinates(birddat)[, 1], 
                                             y = sf::st_coordinates(birddat)[, 2])) +
        geom_path() +
        geom_point(data = birddat, 
                   aes(x = sf::st_coordinates(birddat)[, 1], 
                       y = sf::st_coordinates(birddat)[, 2],
                       colour = behav)) +
        ggtitle(paste0("Behaviour classification of ID ", bird)) +
        xlab("Easting") +
        ylab("Northing")
      print(birdplot)
      
      #Save as artefact
      dev.off()
      
    }
    
  }
  
  
  # Generate summary table
  behavsummary <- table(mt_track_id(updateddata), updateddata$behav)
  write.csv(behavsummary, file = appArtifactPath("behavsummary.csv"))
  
  
  # Return final result
  return(updateddata)
  
}
