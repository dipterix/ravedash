#' @export
register_pipeline <- function(pipeline_name, settings_file = "settings.yaml",
                              env = parent.frame()) {
  if(system.file(package = 'raveio') == ""){
    ravedash::logger("register_pipeline: Cannot register pipeline. Please install `raveio` package", level = "error")
    return()
  }
  if(length(pipeline_name) != 1 || is.na(pipeline_name)) {
    ravedash::logger("register_pipeline: `pipeline_name` is invalid", level = "error")
    return()
  }


  paths <- c("./modules", "./_pipelines")
  default_paths <- c(".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines"))

  paths <- c(paths[dir.exists(paths)], default_paths)

  raveio::pipeline_root(paths)
  pipeline_path <- raveio::pipeline_find(pipeline_name)

  pipeline_settings_path <- file.path(pipeline_path, settings_file)
  settings <- raveio::load_yaml(pipeline_settings_path)
  pipeline_set <- function(..., .list = NULL){
    args <- c(list(...), as.list(.list))
    argnames <- names(args)
    if(!length(argnames)){
      return(as.list(settings))
    }
    args <- args[argnames != ""]
    argnames <- names(args)
    if(!length(argnames)){
      return(as.list(settings))
    }
    for(nm in argnames){
      settings[[nm]] <<- args[[nm]]
    }
    raveio::save_yaml(x = settings, file = pipeline_settings_path, sorted = TRUE)
    return(as.list(settings))
  }
  pipeline_get <- function(key, default = NULL, constraint){
    if(missing(key)){
      return(as.list(settings))
    }
    if(!settings$`@has`(key)){
      re <- default
    } else {
      re <- settings[[key]]
    }
    if(!missing(constraint)){
      re <- re %OF% constraint
    }
    re
  }

  env$pipeline_set <- pipeline_set
  env$pipeline_get <- pipeline_get
  env$pipeline_settings_path <- pipeline_settings_path
  env$pipeline_path <- pipeline_path
}

