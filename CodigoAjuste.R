
current_iteration <- 0

optimize_parameter = function(parameter, iteration_limit, fun_to_optimize) {
  is_parameter_locked <- FALSE

  old_gain <- fun_to_optimize(parameter)

  while((! is_parameter_locked) && current_iteration < iteration_limit) {
    old_parameter <- parameter
    parameter <- ceiling(parameter * 1.1)
    new_gain <- fun_to_optimize(parameter)
    if (new_gain > old_gain) {
      old_gain <- new_gain
    } else {
      parameter <- floor(old_parameter * 0.9)
      new_gain <- fun_to_optimize(parameter)
      if (new_gain > old_gain) {
        old_gain <- new_gain
      }
      else{
        is_parameter_locked <- TRUE
      }
    }
    current_iteration <<- current_iteration + 1
  }
  return(parameter)
}

find_optimal_parameters = function(dataset, pmaxdepth, pminbucket, pminsplit, pcp, iteration_limit )
{
  iteration_limit <- 1000

  ############# DEPTH
  optimal_depth_not_found <- TRUE
  while (current_iteration < iteration_limit && optimal_depth_not_found){
    current_iteration <<- current_iteration + 1
    fun_to_iterate_over=function(value) {
      result <- modelo_rpart_ganancia(dataset, pmaxdepth=value, pminbucket=pminbucket, pminsplit=pminsplit, pcp=pcp )
      return(mean(result$ganancia))
    }
    remaining_iterations <- iteration_limit - current_iteration
    optimal_depth <- optimize_parameter(parameter = pmaxdepth, iteration_limit=remaining_iterations, fun_to_optimize=fun_to_iterate_over)
    ############# BUCKET
    optimal_bucket_not_found <- TRUE
    while (current_iteration < iteration_limit && optimal_bucket_not_found){
      initial_iteration_amount <- current_iteration
      current_iteration <<- current_iteration + 1
      fun_to_iterate_over=function(value) {
        result <- modelo_rpart_ganancia(dataset, pmaxdepth=optimal_depth, pminbucket=value, pminsplit=pminsplit, pcp=pcp )
        return(mean(result$ganancia))
      }
      remaining_iterations <- iteration_limit - current_iteration
      optimal_bucket <- optimize_parameter(parameter = pminbucket, iteration_limit=remaining_iterations, fun_to_optimize=fun_to_iterate_over)
      optimal_bucket_not_found <- FALSE
      if (current_iteration < initial_iteration_amount - 4) {
        optimal_depth_not_found <- TRUE
      }
      ############# SPLIT
      optimal_split_not_found <- TRUE
      while (current_iteration < iteration_limit && optimal_split_not_found){
        initial_iteration_amount <- current_iteration
        current_iteration <<- current_iteration + 1
        fun_to_iterate_over=function(value) {
          result <- modelo_rpart_ganancia(dataset, pmaxdepth=optimal_depth, pminbucket=optimal_bucket, pminsplit=value, pcp=pcp )
          return(mean(result$ganancia))
        }
        remaining_iterations <- iteration_limit - current_iteration
        optimal_split <- optimize_parameter(parameter = pminsplit, iteration_limit=remaining_iterations, fun_to_optimize=fun_to_iterate_over)
        optimal_split_not_found <- FALSE
        if (current_iteration < initial_iteration_amount - 4) {
          optimal_bucket_not_found <- TRUE
        }      
        ############# PCP
        optimal_pcp_not_found <- TRUE
        while (current_iteration < iteration_limit && optimal_pcp_not_found){
          initial_iteration_amount <- current_iteration
          current_iteration <<- current_iteration + 1
          fun_to_iterate_over=function(value) {
            result <- modelo_rpart_ganancia(dataset, pmaxdepth=optimal_depth, pminbucket=optimal_bucket, pminsplit=optimal_split, pcp=pcp )
            return(mean(result$ganancia))
          }
          remaining_iterations <- iteration_limit - current_iteration
          optimal_pcp <- optimize_parameter(parameter = pcp, iteration_limit=remaining_iterations, fun_to_optimize=fun_to_iterate_over)
          optimal_pcp_not_found <- FALSE
          if (current_iteration < initial_iteration_amount - 4) {
            optimal_split_not_found <- TRUE
          }      
        }
        optimal_split_not_found <- FALSE        
      }
      optimal_bucket_not_found <- FALSE
    }
    optimal_depth_not_found <- FALSE
  }
  cat("Depth ", optimal_depth, " Found after ", current_iteration, "\n")
  cat("Bucket ", optimal_bucket, " Found after ", current_iteration, "\n")
  cat("Split ", optimal_split, " Found after ", current_iteration, "\n")
  cat("Pcp ", optimal_pcp, " Found after ", current_iteration, "\n")
}


############################################ OLD VERSION #######################################################
#find_optimal_parameters = function(dataset, pmaxdepth, pminbucket, pminsplit, pcp, iteration_limit )
#{
#  iteration_limit <- 1000
#  optimal_depth_not_found <- TRUE
#
#  while (current_iteration < iteration_limit && optimal_depth_not_found){
#    current_iteration <<- current_iteration + 1
#    fun_to_iterate_over=function(value) {
#      result <- modelo_rpart_ganancia(dataset, pmaxdepth=value, pminbucket=pminbucket, pminsplit=pminsplit, pcp=pcp )
#      return(mean(result$ganancia))
#    }
#    remaining_iterations <- iteration_limit - current_iteration
#    optimal_depth <- optimize_parameter(parameter = pmaxdepth, iteration_limit=remaining_iterations, fun_to_optimize=fun_to_iterate_over)
#    optimal_bucket_not_found <- TRUE
#    while (current_iteration < iteration_limit && optimal_bucket_not_found){
#      initial_iteration_amount <- current_iteration
#      current_iteration <<- current_iteration + 1
#      fun_to_iterate_over=function(value) {
#        result <- modelo_rpart_ganancia(dataset, pmaxdepth=optimal_depth, pminbucket=value, pminsplit=pminsplit, pcp=pcp )
#        return(mean(result$ganancia))
#      }
#      remaining_iterations <- iteration_limit - current_iteration
#      optimal_bucket <- optimize_parameter(parameter = pminbucket, iteration_limit=remaining_iterations, fun_to_optimize=fun_to_iterate_over)
#      optimal_bucket_not_found <- FALSE
#      if (current_iteration < initial_iteration_amount - 4) {
#        optimal_depth_not_found <- TRUE
#      }
#    }
#    optimal_depth_not_found <- FALSE
#  }
#  cat("Depth ", optimal_depth, " Found after ", current_iteration, "\n")
#  cat("Bucket ", optimal_bucket, " Found after ", current_iteration, "\n")
#} 
#############################################################