library('move2')
require('dplyr')
require('magrittr')
library('lubridate')
library('data.table')
library('splines')
library('spam')

is_tibble <- tibble::is_tibble # required below
`%!in%` <- Negate(`%in%`) # 'Not In' function

# logger
source("src/common/logger.R")

# Define inputs
data <- readRDS("data/raw/input6.rds")
travelcut <- 3
rooststart <- 18
roostend <- 7

# Call modelfile 
providedModels <- readRDS("data/local_app_files/uploaded-app-files/second_stage_model/modelfit.rds")
standardModel <- readRDS("data/local_app_files/provided-app-files/standard_model/standard_model.rds")
nomodels <- FALSE



# ------------
# 1. Check input data is in correct format
# ------------

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


# ------------
# 2. Perform general classification
# ------------


data %<>% 
  arrange(mt_track_id(data), mt_time(data)) %>%
  # distinct(timestamp, .keep_all = TRUE) %>%
  
  mutate(
    dist_m = sqrt((x - lag(x, default = NA))^2 + (y - lag(y, default = NA))^2),
    timediff_hrs = as.numeric(difftime(mt_time(data), lag(mt_time(data), default = mt_time(data)[1]), units = "hours")), 
    kmph = dist_m/c(timediff_hrs*1000), 
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
    )
  )  %>%
  mutate(stationary = case_when(
    as.vector(dist_m) < travelcut & between(hour(timestamp), roostend, rooststart) ~ 1,
    TRUE ~ 0
  ))



logger.info("First stage classification complete")

# Debug
print(table(data$behav))


# ------------
# 3. Accelerometer Data
# ------------

# Accelerometer operations will go here
# Ignoring in first instance for Savannah tag test
#}


# ------------
# 4. Second-stage updating (if data available)
# ------------


# We fit the second-stage model for each animal, and then re-stack the data into a move2 object
newdat <- list()


for (tag in unique(mt_track_id(data))) {
  
  # If there is no data for this bird, skip its second-stage
  birddat <- filter_track_data(data, .track_id = tag)
  if(nrow(birddat) == 0) {
    logger.info(paste0("No data exists for ID ", tag, " with associated model. Skipping second-stage classification"))
    next}
  
  logger.info(paste0("Beginning second-stage classification for ID ", tag))
  
  # Retrieve model
  if (tag %in% names(providedModels) & nomodels == FALSE) {
    # If animal has an associated model, call it:
    fit <- providedModels[[tag]]
  } else {
    # If no model provided for this bird, use the fallback model:
    logger.warn(paste0("No model has been fitted to ID ", tag, " . Applying standard fallback model for Gyps Africanus vultures"))
    fit <- standardModel
  }
  
  
  # If indexes aren't already added by preprocessing MoveApp, include them (and remove again later)
  if("index" %!in% colnames(birddat)) {
    removelater <- TRUE
    birddat %<>% mutate(
      index = paste0(mt_track_id(birddat), " ", mt_time(birddat))
    ) %>%
      select(any_of(c("index", "behav", "stationary", "hourmin", "kmph", "yearmonthday", mt_track_id_column(birddat), mt_time_column(birddat))))
  } else {removelater <- FALSE}
  
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
  
  # -- Get predictions and Construct confidence intervals
  
  x_values <- sort(unique(birddat$hourmin))
  b <- cbind(rep(1, length(x_values)), splines::bs(x_values, 
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
    empPval[i]<- length(which(predint[ which(x_values==data$hourmin[i]), ] < birddat$kmph[data$hourmin==data$hourmin[i] & data$yearmonthday==data$yearmonthday[i]]))/1000
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


# Rejoin move2 objects into classified data:
logger.info("All second-stage classifications complete. Re-stacking to move2 object")
updateddata <- move2::mt_stack(newdat)

# Remove index if not present at start
if(removelater == TRUE) {
  updateddata %<>% select(-index)
}

# Debug
print(table(updateddata$behav))












