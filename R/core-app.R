session_root <- function(ensure = FALSE){
  cache_path <- raveio::raveio_getopt("tensor_temp_path", default = file.path(tempdir(), "rave2-session"))
  if( ensure && !dir.exists(cache_path) ){
    raveio::dir_create2(cache_path)
  }
  normalizePath(cache_path, mustWork = FALSE)
}

ensure_template <- function(path, use_cache = TRUE){
  template_path <- file.path(R_user_dir("ravedash", 'data'), 'themes', 'rave-pipelines')
  backup_path <- file.path(R_user_dir("ravedash", 'data'), 'backups')
  backup_template_path <- file.path(backup_path, 'rave-pipelines')

  if(!use_cache || !dir.exists(template_path)){
    if(dir.exists(template_path)){
      raveio::dir_create2(backup_path)
      if(dir.exists(backup_template_path)){
        unlink(backup_template_path, recursive = TRUE)
      }
      file.copy(template_path, backup_path, recursive = TRUE, overwrite = TRUE)
      unlink(template_path, recursive = TRUE)
    }
    shidashi::use_template(path = template_path, user = 'dipterix', theme = 'rave-pipelines', branch = "master")
    if(dir.exists(backup_template_path)){
      unlink(backup_template_path, recursive = TRUE)
      if(!length(list.files(backup_path))){
        unlink(backup_path, recursive = TRUE)
      }
    }
  }

  fs <- list.files(template_path, full.names = TRUE)
  raveio::dir_create2(path)
  for(f in fs){
    file.copy(f, to = path, overwrite = TRUE, copy.date = TRUE, recursive = TRUE)
  }
  normalizePath(path)
}

#' @export
new_session <- function(update = FALSE) {

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
  session_id <- paste0(strftime(Sys.time(), "session-%y%m%d-%M%H%S-%Z-"), toupper(rand_string(4)))
  cache_path <- session_root(ensure = TRUE)
  cache_path <- normalizePath(cache_path)
  app_path <- file.path(cache_path, session_id)

  # raveio::dir_create2(app_path)
  ensure_template(app_path, use_cache = !update)

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

  use_session(session_id)

}

#' @export
use_session <- function(session_id) {
  cache_path <- session_root()
  app_path <- file.path(cache_path, session_id)
  app_path <- normalizePath(app_path, mustWork = TRUE)

  structure(
    list(
      launch_app = function(...){
        shidashi::render(root_path = app_path, ...)
      },
      session_id = session_id
    ),
    class = "rave-dash-session"
  )
}

#' @export
`print.rave-dash-session` <- function(x, ...){
  vname <- substitute(x)
  session_id <- x$session_id
  session_path <- file.path(session_root(), session_id)
  cat(sprintf("RAVE session <%s>\n", session_id))
  timestamp <- sub("\\-[a-zA-Z0-9]{4}$", "", session_id)
  timestamp <- strsplit(timestamp, "-")[[1]]
  tz <- timestamp[[4]]
  timestamp <- paste(timestamp[c(2,3)], collapse = "-")
  timestamp <- strptime(timestamp, "%y%m%d-%H%M%S")
  timestamp <- strftime(timestamp, usetz = FALSE)
  timestamp <- paste(timestamp, tz)

  if(dir.exists(session_path)){
    cat("  Path:", session_path, "\n")
  } else {
    cat("  Path:", session_path, "(invalid path)\n")
  }
  cat("  Date created:", timestamp, "\n\n")

  vname <- paste(deparse(vname), collapse = "\n")
  cat(sprintf("Please run `%s$launch_app()` to launch the session.\n", vname))

}

#' @export
remove_session <- function(session_id){
  if(grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", session_id)){
    session_path <- file.path(session_root(), session_id)
    if(dir.exists(session_path)){
      unlink(session_path, recursive = TRUE)
      return(invisible(TRUE))
    }
  }
  return(invisible(FALSE))
}

#' @export
list_session <- function(path = session_root()){
  dirs <- list.dirs(path = path, full.names = FALSE, recursive = FALSE)
  sel <- grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", dirs)
  lapply(dirs[sel], use_session)
}
