# Creating fallback auxiliary file for second-stage classification
# Temp fix: calling random model 


selectedmodel <- "NO MODEL PROVIDED. All IDs will be classified using the generalised model"

saveRDS(selectedmodel, "data/local_app_files/provided-app-files/second_stage_model/modelfit.rds")
