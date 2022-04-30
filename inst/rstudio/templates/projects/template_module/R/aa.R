library(ravedash)
# global variables for the module

# Stores global variables. These are required
pipeline_name <- "{{PIPELINE_NAME}}"
pipeline_settings_file <- "settings.yaml"
module_id <- "{{MODULE_ID}}"
debug <- TRUE

#' Function to check whether data is loaded.
#' @param first_time whether this function is run for the first time
#' @details The function will be called whenever \code{data_changed} event is
#' triggered. This function should only return either \code{TRUE} or
#' \code{FALSE} indicating the check results. If \code{TRUE} is returned,
#' \code{module_html} will be called, and module 'UI' should be displayed.
#' If \code{FALSE} is returned, \code{open_loader} event will be dispatched,
#' resulting in calling function \code{loader_html}.
#' @return Logical variable of length one.
check_data_loaded <- function(first_time = FALSE){
  FALSE
}



# ----------- Initial configurations -----------

# Change the logger level when `debug` is enabled
if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravedash::logger_threshold("trace", module_id = module_id)
} else {
  ravedash::logger_threshold("info", module_id = module_id)
}

# Register RAVE pipeline, this will give you:
# `pipeline_set`: set variables in the pipeline
# `pipeline_get`: get variables in the pipeline
# `pipeline_settings_path`: where the settings file locate
# `pipeline_path`: parent directory path of the pipeline
register_pipeline(pipeline_name = pipeline_name,
                  settings_file = pipeline_settings_file,
                  env = environment())


