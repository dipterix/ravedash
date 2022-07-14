session_root <- function(ensure = FALSE){
  cache_path <- raveio::raveio_getopt("tensor_temp_path", default = file.path(tempdir(), "rave2-session"))
  if( ensure && !dir.exists(cache_path) ){
    raveio::dir_create2(cache_path)
  }
  normalizePath(cache_path, mustWork = FALSE)
}

ensure_template <- function(path, use_cache = TRUE){


  template_path <- file.path(R_user_dir("raveio", 'data'), 'rave-pipelines')

  if(!use_cache || !dir.exists(template_path)) {
    raveio::pipeline_install_github('dipterix/rave-pipelines', to = "default")
  }

  fs <- list.files(template_path, full.names = TRUE, recursive = FALSE)
  raveio::dir_create2(path)
  for(f in fs){
    file.copy(f, to = path, overwrite = TRUE, copy.date = TRUE, recursive = TRUE)
  }
  # remove <path>/modules
  module_path <- file.path(path, "modules")
  if(dir.exists(module_path)) {
    unlink(module_path, recursive = TRUE)
    raveio::dir_create2(module_path)
  }
  module_yaml_path <- file.path(path, "modules.yaml")
  if(file.exists(module_yaml_path)) {
    unlink(module_yaml_path)
  }
  normalizePath(path)
}

#' @name rave-session
#' @title Create, register, list, and remove 'RAVE' sessions
#' @param update logical, whether to update to latest 'RAVE' template
#' @param session,x session identification string, or session object; use
#' \code{list_session} to list all existing sessions
#' @param path root path to store the sessions; default is the
#' \code{"tensor_temp_path"} in \code{\link[raveio]{raveio_getopt}}
#' @param host host 'IP' address, default is 'localhost'
#' @param port port to listen
#' @param options additional options, including \code{jupyter},
#' \code{jupyter_port}, \code{as_job}, and \code{launch_browser}
#' @param jupyter logical, whether to launch 'jupyter' instance as well. It
#' requires additional setups to enable 'jupyter' lab
#' @param jupyter_port port used by 'jupyter' lab, can be set by
#' \code{'jupyter_port'} option in \code{\link[raveio]{raveio_setopt}}
#' @param as_job whether to launch the application as 'RStudio' job, default is
#' true if 'RStudio' is detected; when running without 'RStudio', this option
#' is always false
#' @param launch_browser whether to launch browser, default is true
#' @param new whether to create a new session instead of using the most recent
#' one, default is false
#' @param order whether to order the session by date created; choices are
#' \code{'none'} (default), \code{'ascend'}, \code{'descend'}
#' @return
#' \describe{
#' \item{\code{new_session}}{returns a session object with character
#' \code{'session_id'} and a function \code{'launch_session'} to launch the
#' application from this session}
#' \item{\code{use_session}}{returns a session object, the same as
#' \code{new_session} under the condition that corresponding session exists,
#' or raise an error if the session is missing}
#' \item{\code{list_session}}{returns a list of all existing session objects
#' under the session root}
#' \item{\code{remove_session}}{returns a logical whether the corresponding
#' session has been found and removed}
#' }
#'
#' @examples
#'
#' if(interactive()){
#'
#'   sess <- new_session()
#'   sess$launch_session()
#'
#'   all_sessions <- list_session()
#'   print(all_sessions)
#'
#'   # Use existing session
#'   session_id <- all_sessions[[1]]$session_id
#'   sess <- use_session(session_id)
#'   sess$launch_session()
#'
#'   # Remove session
#'   remove_session(session_id)
#'   list_session()
#' }
#'
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
  session_id <- paste0(strftime(Sys.time(), "session-%y%m%d-%H%M%S-%Z-"), toupper(rand_string(4)))
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

  module_conf <- raveio::load_yaml(file.path(module_root_path, "modules.yaml"))
  groups <- lapply(module_conf$modules, function(item){
    if(length(item$group) == 1) {
      order <- c(item$order, 99999L)
      order <- min(order, na.rm = TRUE)
      return(data.frame(
        group = item$group,
        order = order
      ))
    }
  })
  groups <- do.call("rbind", dipsaus::drop_nulls(groups))
  groups <- lapply(split(groups, groups$group), function(item){
    list(
      name = item$group[[1]],
      order = min(item$order),
      open = FALSE
    )
  })
  names(groups) <- sapply(groups, '[[', 'name')
  module_conf$groups <- groups

  if(!is.list(module_conf$divider)) {
    module_conf$divider <- list()
  }
  module_conf$divider[["Preprocess"]] <- list(order = 0)
  module_conf$divider[["Built-ins"]] <- list(order = 9.99)
  module_conf$divider[["Add-ons"]] <- list(order = 99.99)

  raveio::save_yaml(module_conf, file = file.path(app_path, "modules.yaml"))
  # file.copy(
  #   from = file.path(module_root_path, "modules.yaml"),
  #   to = file.path(app_path, "modules.yaml"),
  #   overwrite = TRUE, copy.date = TRUE
  # )

  logger("A new RAVE session has been created [session ID: { session_id }]", level = "info", use_glue = TRUE)
  use_session(session_id)

}

#' @rdname rave-session
#' @export
use_session <- function(x) {
  UseMethod("use_session")
}

#' @export
use_session.default <- function(x) {
  force(x)

  if(length(x) != 1 || is.na(x) || !grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", x)) {
    stop("Invalid session ID")
  }

  cache_path <- session_root()
  app_path <- file.path(cache_path, x)
  app_path <- normalizePath(app_path, mustWork = TRUE)

  x <- structure(
    list(
      app_path = app_path,
      session_id = x
    ),
    class = "rave-dash-session"
  )
  x$launch_session <- function(...) {
    launch_session(x, ...)
  }
  x
}

#' @rdname rave-session
#' @export
launch_session <- function(
    x, host = "127.0.0.1", port = NULL, options = list(
      jupyter = TRUE,
      jupyter_port = NULL,
      as_job = TRUE,
      launch_browser = TRUE
    )) {
  sess <- use_session(x)

  default_options <- list(
    jupyter = TRUE,
    jupyter_port = NULL,
    as_job = TRUE,
    launch_browser = TRUE
  )

  for(nm in names(options)) {
    default_options[[nm]] <- options[[nm]]
  }

  options <- default_options

  jupyter_port <- options$jupyter_port

  if(isTRUE(options$jupyter)) {
    if(length(jupyter_port) == 0) {
      jupyter_port <- raveio::raveio_getopt("jupyter_port", default = 17284L)
    } else {
      jupyter_port <- as.integer(jupyter_port)
      if(!(length(jupyter_port) == 1 && !is.na(jupyter_port) &&
           jupyter_port >= 1024 && jupyter_port <= 65535)) {
        stop("`launch_session`: options$jupyter_port must be an integer from 1024-65535.")
      }
    }
    jupyter_wd <- raveio::raveio_getopt('data_dir')
    rpymat::jupyter_check_launch(
      open_browser = FALSE, workdir = jupyter_wd, port = jupyter_port,
      host = host, async = TRUE)

    raveio::save_yaml(
      list(
        host = host,
        port = jupyter_port
      ),
      file = file.path(x$app_path, "jupyter.yaml")
    )
  }
  port <- as.integer(port)
  if(length(port)) {
    if(!(length(port) == 1 && !is.na(port) && port >= 1024 && port <= 65535)) {
      stop("`launch_session`: port must be an integer from 1024-65535.")
    }
  } else {
    port <- NULL
  }

  shidashi::render(
    root_path = x$app_path,
    port = port,
    host = host,
    launch_browser = options$launch_browser,
    as_job = options$as_job,
    test_mode = isTRUE(options$test_mode)
  )

}


#' @export
`use_session.rave-dash-session` <- function(x) {
  x
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
  cat(sprintf("Please run `%s$launch_session()` to launch the session.\n", vname))

  invisible(x)
}

#' @rdname rave-session
#' @export
remove_session <- function(x){
  UseMethod("remove_session")
}

#' @export
remove_session.default <- function(x){
  if(grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", x)){
    session_path <- file.path(session_root(), x)
    if(dir.exists(session_path)){
      unlink(session_path, recursive = TRUE)
      return(invisible(TRUE))
    }
  }
  return(invisible(FALSE))
}

#' @export
`remove_session.rave-dash-session` <- function(x) {
  if(grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", x$session_id)){
    session_path <- file.path(session_root(), x$session_id)
    if(dir.exists(session_path)){
      unlink(session_path, recursive = TRUE)
      return(invisible(TRUE))
    }
  }
  return(invisible(FALSE))
}

#' @rdname rave-session
#' @export
remove_all_sessions <- function() {
  sess <- list_session()
  for(s in sess) {
    remove_session(s)
  }
  invisible()
}

#' @rdname rave-session
#' @export
list_session <- function(path = session_root(), order = c("none", "ascend", "descend")){
  order <- match.arg(order)
  dirs <- list.dirs(path = path, full.names = FALSE, recursive = FALSE)
  sel <- grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", dirs)
  session_ids <- dirs[sel]
  if(order != "none") {

    timestamps <- sub("\\-[a-zA-Z]+\\-[a-zA-Z0-9]{4}$", "", session_ids)
    timestamps <- strptime(timestamps, "session-%y%m%d-%H%M%S")
    order <- order(timestamps, decreasing = order == "descend", na.last = TRUE)
    session_ids <- session_ids[order]

  }
  re <- lapply(session_ids, use_session)
  re
}

#' @rdname rave-session
#' @export
start_session <- function(session, new = FALSE, host = "127.0.0.1", port = NULL,
                          jupyter = NA, jupyter_port = NULL, as_job = TRUE,
                          launch_browser = TRUE) {
  if(!missing(session) && length(session)) {
    if(new) {
      stop("`start_session`: Please leave `session` blank or NULL if you want to create a new session (new=TRUE).")
    }
    if(length(session) != 1) {
      stop("`start_session`: Invalid `session`.")
    }
    if(!inherits(session, "rave-dash-session")) {
      if(!grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", session)) {
        stop("`start_session`: Invalid `session`.")
      }
      session <- tryCatch({
        use_session(x = session)
      }, error = function(e) {
        stop(sprintf("`start_session`: Session [%s] cannot be found.", session))
      })
    }
  } else if(new){
    session <- new_session()
  } else {
    # find existing session (most recent)
    all_sessions <- list_session(order = "descend")
    if(length(all_sessions)) {
      session <- all_sessions[[1]]
    } else {
      # start a new session
      session <- new_session()
    }
  }

  rs_available <- dipsaus::rs_avail(child_ok = TRUE, shiny_ok = TRUE)
  if(is.na(jupyter)) {
    jupyter <- rs_available
  } else if(isTRUE(jupyter) && !rs_available) {
    warning("RStudio is not available. Please manually launch Jupyter lab")
    jupyter <- FALSE
  }
  if(!rs_available) {
    as_job <- FALSE
  }

  if(as_job) {
    job_id <- launch_session(x = session, host = host, port = port, options = list(
      jupyter = jupyter,
      jupyter_port = jupyter_port,
      as_job = as_job,
      launch_browser = launch_browser
    ))
    if(as_job) {
      logger("RAVE application [{session$session_id}] has been launched. Detailed information has been printed out in the `jobs` panel.", level = "info", use_glue = TRUE, .trim = FALSE)
    }

    return(invisible(list(
      session = session,
      job_id = job_id
    )))
  } else {
    return(launch_session(x = session, host = host, port = port, options = list(
      jupyter = jupyter,
      jupyter_port = jupyter_port,
      as_job = as_job,
      launch_browser = launch_browser
    )))
  }
}
