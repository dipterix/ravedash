session_root <- function(ensure = FALSE){
  cache_path <- raveio::raveio_getopt("tensor_temp_path", default = file.path(tempdir(), "rave2-session"))
  if( ensure && !dir.exists(cache_path) ){
    raveio::dir_create2(cache_path)
  }
  normalizePath(cache_path, mustWork = FALSE)
}

ensure_template <- function(path){
  template_path <- file.path(R_user_dir("ravedash", 'data'), 'themes', 'AdminLTE3-rave')
  if(!dir.exists(template_path)){
    shidashi::use_template(path = template_path, user = 'dipterix', theme = 'AdminLTE3-rave')
  }

  fs <- list.files(template_path, full.names = TRUE)
  raveio::dir_create2(path)
  for(f in fs){
    file.copy(f, to = path, overwrite = TRUE, copy.date = TRUE, recursive = TRUE)
  }
  normalizePath(path)
}

new_session <- function() {

  # o <- raveio::pipeline_root()
  # on.exit({
  #   raveio::pipeline_root(o)
  # })

  src_root <- R_user_dir("raveio", 'data')
  src_pipeline <- file.path(R_user_dir("raveio", 'data'), "pipelines")

  if(!dir.exists(src_pipeline)){
    stop("No pipeline found. TODO (dev: fixe this)")
  }
  pipelines <- raveio::pipeline_list(root_path = src_pipeline)
  if(!length(pipelines)){
    stop("No pipeline found. TODO (dev: fixe this)")
  }

  # create a temporary repository
  rave_id <- paste0(strftime(Sys.time(), "session-%y%m%d-%M%H%S-%Z-"), toupper(rand_string(4)))
  cache_path <- session_root(ensure = TRUE)
  cache_path <- normalizePath(cache_path)

  app_path <- file.path(cache_path, rave_id)

  # raveio::dir_create2(app_path)
  ensure_template(app_path)

  pipeline_path <- file.path(app_path, "_pipelines")

  for(pipeline in pipelines){
    p <- raveio::pipeline_find(pipeline)
    raveio::pipeline_fork(
      src = p, dest = file.path(pipeline_path, pipeline)
    )
  }

  # copy modules
  module_root_path <- file.path(R_user_dir("raveio", 'data'), "shidashi_modules")
  source_path <- file.path(module_root_path, "modules")
  target_path <- file.path(app_path, "modules")
  if(file.exists(target_path)){
    unlink(target_path, recursive = TRUE)
  }
  file.copy(
    from = source_path, to = app_path,
    recursive = TRUE, copy.date = TRUE
  )

  file.copy(
    from = file.path(module_root_path, "modules.yaml"),
    to = file.path(app_path, "modules.yaml"),
    overwrite = TRUE, copy.date = TRUE
  )

  use_session(rave_id)

}

use_session <- function(rave_id) {
  cache_path <- session_root()
  app_path <- file.path(cache_path, rave_id)
  app_path <- normalizePath(app_path, mustWork = TRUE)

  list(
    launch_app = function(...){
      shidashi::render(root_path = app_path, ...)
    }
  )
}

remove_session <- function(rave_id){
  if(grepl("^rave-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", rave_id)){
    session_path <- file.path(session_root(), rave_id)
    if(dir.exists(session_path)){
      unlink(session_path, recursive = TRUE)
      return(invisible(TRUE))
    }
  }
  return(invisible(FALSE))
}

list_session <- function(path = session_root()){
  dirs <- list.dirs(path = path, full.names = FALSE, recursive = FALSE)
  sel <- grepl("^rave-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", dirs)
  dirs[sel]
}
