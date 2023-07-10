library('move2')
library('lubridate')
library('magrittr')
library('dplyr')
library('MRSea')
library('splines')
library('spam')


## The parameter "data" is reserved for the data object passed on from the previous app

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

`%!in%` <- Negate(`%in%`) # 'Not In' function

# Showcase injecting app setting (parameter `year`)
rFunction = function(data, rooststart, roostend, travelcut, second_stage_model = NULL) {


  # Prepare Second-Stage Models --------------------------------------------------------------------------------------------
  
  # Call model file, if provided:
  modelPath <- paste0(getAppFilePath("second_stage_model", fallbackToProvidedFiles = TRUE), "modelfit.rds")
  providedModels <- readRDS(modelPath)
  
  if(is.character(providedModels)) {
    nomodels <- TRUE
    logger.warn(providedModels)
    } else {
    nomodels <- FALSE
    logger.info(paste0("Models provided for tags ", toString(names(providedModels)), ". Proceeding with classification"))
  }
  
  # Call standard classification model file, for all IDs not included in the above file:
  standardModelPath <- paste0(getAppFilePath("standard_model", fallbackToProvidedFiles = TRUE), "standard_model.rds")
  standardModel <- readRDS(standardModelPath)
  
  
  
  # Check Input Data ----------------------------------------------------------------------------------------------

  if(nrow(data) == 0) {
    logger.warn("Input data is empty. Returning input.")
    return(data)}
  
  needcols <- c("timestamp", "x", "y")
  
  if(!all(needcols %in% colnames(data))) {
    missingcols <- which(needcols %!in% colnames(data))
    logger.fatal(paste0("Missing essential data columns: ", toString(needcols[missingcols])))
    logger.fatal("Input data is not in correct format. Use 'preprocessing' MoveApp before this step in the workflow. Returning input")
    return(data)
  }
  
  
  logger.info("Input is in correct format. Proceeding with first-stage classification for all IDs")
  
  
  # First-Stage Classification --------------------------------------------------------------------------------------
  
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
  
  
  #'- First-stage classification
  #' Using kmph as a measure instead of dist_m
  #' 
  data %<>%
    mutate(
      behav = case_when(
        kmph < travelcut & !data.table::between(hour, roostend, rooststart, incbounds = FALSE) ~ "SRoosting",
        kmph < travelcut & data.table::between(hour, roostend, rooststart, incbounds = FALSE)  ~ "SResting",
        kmph > travelcut                                            ~ "STravelling",
        TRUE                                                    ~ "Unknown"
      ),
      stationary = ifelse(behav=="STravelling", 0, 1)
    )
    # )  %>%
    # 
    # mutate(stationary = case_when(
    #                     stationary = ifelse(behav=="STravelling", 0, 1), 
    # 
    #   as.vector(dist_m) < travelcut & between(hour(mt_time(.)), roostend, rooststart) ~ 1,
    #   TRUE ~ 0
    # ))
    # 
  
  
  logger.info("First stage classification complete")
  
  
  # Accelerometer Data -----------------------------------------------------------------------------------------------
  
  # Accelerometer operations will go here
  # Ignoring in first instance for Savannah tag test
  #}
  
  
  # Second-Stage Classification ----------------------------------------------------------------------------------
  
  
  # We fit the second-stage model for each animal, and then re-stack the data into a move2 object
  newdat <- list()
  

  for (tag in unique(mt_track_id(data))) {
    
    # If there is not enough data for this bird, skip its second-stage
    birddat <- filter_track_data(data, .track_id = tag)
    if(nrow(birddat) < 10) {
      logger.info(paste0("Insufficient data exists for ID ", tag, " with associated model. Skipping second-stage classification"))
      next}
    
    logger.info(paste0("Beginning second-stage classification for ID ", tag))
    
    
    ## Calling model ==========================================================================================
    
    # Retrieve model
    if (tag %in% names(providedModels) & nomodels == FALSE) {
      # If animal has an associated model, call it:
      fit <- providedModels[[tag]]
    } else {
      # If no model, use the fallback model:
      logger.warn(paste0("No model has been fitted to ID ", tag, " . Applying fallback model for Gyps Africanus vultures"))
      fit <- standardModel
    }
    
    
    ## Indexing =============================================================================================
    
    # If indexes aren't already added by preprocessing MoveApp, include them (and remove again later)
    if("index" %!in% colnames(birddat)) {
      removelater <- TRUE
      birddat %<>% mutate(
        index = paste0(mt_track_id(birddat), " ", mt_time(birddat))
      ) %>%
        select(any_of(c("index", "behav", "stationary", "hourmin", "kmph", "yearmonthday", mt_track_id_column(birddat), mt_time_column(birddat))))
    } else {removelater <- FALSE}
    
    
    ## Cumulative stationary time =========================================================================
    
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
    
    
    ## Movement reclassification ================================================================================
    
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
        behav = ifelse(cumtimestat_pctl < 0.05 & behav == "SResting", "SFeeding", behav)
      ) 
    
    # The following is definitely not the best way to re-join the objects
    # But I can't seem to make sp.join work - this is a temporary fix
    newdat[[tag]] <- birddat
    logger.info(paste0("Second-stage classification complete for ID ", tag))
  }
  
  
  # Return data to move2 object -----------------------------------------------------------------------------------------------
  
  # Rejoin move2 objects into classified data:
  logger.info("All second-stage classifications complete. Re-stacking to move2 object")
  updateddata <- move2::mt_stack(newdat)
  
  # Remove index if not present at start
  if(removelater == TRUE) {
    updateddata %<>% select(-index)
  }
  

  
  # provide my result to the next app in the MoveApps workflow
  result <- updateddata
  return(result)
}
