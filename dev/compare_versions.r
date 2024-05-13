#' Utility function to run and compare the outputs of two competing versions of
#' the App
#' 
#' @param dt a move2 object to be passed on as input for the rFunctions() under
#'   comparison
#' @param f_old the (non-quoted) name of the original version of the rFunction()
#' @param f_new the (non-quoted) name of the modified version of the rFunction()
#' @param fun_new_label,fun_old_label character strings providing the label for
#'   each of the competing code versions
#' @param artifacts_path character string, the path to the Apps output folder
#'   from the current workspace
#' @param return_output logical, whether to return the output datasets produced from 
#' @param ... additional arguments passed on to the competing rFunctions


compare_versions <- function(dt, 
                             f_old, 
                             f_new,
                             fun_new_label = "new",
                             fun_old_label = "old",
                             artifacts_path = "./data/output/",
                             return_output = FALSE,
                             ...
                             ){
  
  withr::with_envvar(
    c("APP_ARTIFACTS_DIR" = artifacts_path), 
    code = {
      
      message("\nRunning old version...\n")
      
      out_old <- f_old(data = dt, ...)|> 
        as_tibble() |> 
        mutate(fun_version = fun_old_label)
      
      message("\nRunning new version...\n")
      gc()
      
      out_new <- f_new(data = dt, ...) |> 
        as_tibble() |> 
        mutate(fun_version = fun_new_label)
    })
  
  out <- bind_rows(out_old, out_new)
  
  stnry_vs_behav <- out |> 
    group_by(fun_version, stationary, behav) |> 
    tally() |> 
    pivot_wider(names_from = behav, values_from = n)
  
  
  classif_contrast <- out |> 
    group_by(fun_version, behav) |> 
    tally() |> 
    pivot_wider(id_cols = behav, names_from = fun_version, values_from = n) |> 
    mutate(pctg_change = 100 * (.data[[fun_new_label]] - .data[[fun_old_label]])/.data[[fun_old_label]])

  
  if(return_output){
    list(
      out_old = out_old,
      out_new =  out_new,
      stnry_vs_behav = stnry_vs_behav,
      classif_contrast = classif_contrast
    )
  }else{
    list(
      stnry_vs_behav = stnry_vs_behav,
      classif_contrast = classif_contrast
    )
  }
  

}