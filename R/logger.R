#' @name logger
#' @title Logger system used by 'RAVE'
#' @description Keep track of messages printed by modules
#' @param ...,.envir,.sep passed to \code{\link[raveio]{glue}}, if
#' \code{use_glue} is true; in \code{log_threshold}, \code{...} are
#' passed to \code{\link[logger]{log_threshold}}
#' @param use_glue whether to use \code{\link[raveio]{glue}} to combine
#' \code{...}; default is false
#' @param calc_delta whether to calculate time difference between current
#' message and previous message; default is \code{'auto'}, which prints
#' time difference when \code{level} is \code{'debug'}.
#' This behavior can be changed by altering \code{calc_delta} by a logical
#' \code{TRUE} to enable or \code{FALSE} to disable.
#' @param level the level of message, choices are \code{'info'} (default),
#' \code{'warning'}, \code{'error'}, \code{'fatal'}, \code{'debug'},
#' \code{'trace'}
#' @param module_id 'RAVE' module identification string, or name-space; default
#' is \code{'ravedash'}
#' @param reset_timer whether to reset timer used by \code{calc_delta}
#' @return The message without time-stamps
#'
#' @examples
#' logger("This is a message")
#'
#' a <- 1
#' logger("A message with glue: a={a}")
#'
#' logger("A message without glue: a={a}", use_glue = FALSE)
#'
#'
#' logger("Message A", calc_delta = TRUE, reset_timer = TRUE)
#' logger("Seconds before logging another message", calc_delta = TRUE)
#'
#'
#' # by default, debug and trace messages won't be displayed
#' logger('debug message', level = 'debug')
#'
#' # adjust logger level, make sure `module_id` is a valid RAVE module ID
#' logger_threshold('debug', module_id = NULL)
#'
#' # Debug message will display
#' logger('debug message', level = 'debug')
#'
#' # Trace message will not display as it's lower than debug level
#' logger('trace message', level = 'trace')
#'
#'
#' @export
logger <- local({
  last_time <- NULL

  show_time_diff <- FALSE
  use_crayon_ <- NA

  use_crayon <- function(){
    if(is.na(use_crayon_)){
      use_crayon_ <<- as.logical(dipsaus::package_installed("crayon"))
    }
    use_crayon_ && crayon::has_color()
  }

  bold <- function(...){
    if(use_crayon()){
      crayon::bold(...)
    } else {
      paste(...)
    }
  }
  colorize <- function(msg, level){
    if(use_crayon()){
      logger::colorize_by_log_level(msg, level)
    } else {
      paste(msg, collapse = " ")
    }
  }

  format_color <- function(msg, fmt, color = "silver") {
    if(!missing(fmt)){
      msg <- format(msg, fmt)
    }
    if(use_crayon()){
      ns <- asNamespace("crayon")
      msg <- ns[[color]](msg)
    }
    msg
  }

  get_delta <- function(delta){
    if(length(delta) != 1){return(NULL)}
    if(delta < 0.3){
      delta_color <- "silver"
    } else if(delta < 0.5){
      delta_color <- "cyan"
    } else if (delta < 1){
      delta_color <- "green"
    } else if (delta < 5){
      delta_color <- "yellow"
    } else {
      delta_color <- "red"
    }
    format_color(msg = sprintf("(+%.2fs)", delta), color = delta_color)
  }

  rave_logger_layout <- function (level, msg, namespace = NA_character_,
                                  .logcall = sys.call(),
                                  .topcall = sys.call(-1),
                                  .topenv = parent.frame()) {

    var <- logger::get_logger_meta_variables(
      log_level = level,
      namespace = namespace,
      .logcall = .logcall,
      .topcall = .topcall,
      .topenv = .topenv
    )

    now <- var$time
    delta <- NULL
    if(show_time_diff){
      if(is.null(last_time)){
        delta <- 0
      } else {
        delta <- dipsaus::time_delta(last_time, now)
      }
      last_time <<- now
    }

    ns <- trimws(var$ns)
    if(!is.na(ns) && !ns %in% c("", "global", "ravedash") ){
      ns_delta <- c(format_color(ns), get_delta(delta))
    } else {
      ns_delta <- get_delta(delta)
    }

    paste(c(
      bold(colorize(msg = var$level, level = var$levelr)),
      format_color(var$time, "%Y-%m-%d %H:%M:%S"),
      ns_delta,
      colorize(msg = paste(msg, collapse = " "), level = var$levelr)
    ), sep = " ", collapse = " ")

  }


  function(..., level = c("info", "warning", "error", "fatal", "debug", "trace"),
           calc_delta = 'auto', .envir = parent.frame(), .sep = "",
           use_glue = FALSE, reset_timer = FALSE){
    level <- match.arg(level)
    module <- get_active_module_info()
    if(is.list(module)){
      namespace <- module$id
    } else {
      namespace <- "ravedash"
    }

    if(identical(calc_delta, "auto") && level %in% c("debug")){
      calc_delta <- TRUE
    }
    show_time_diff <<- isTRUE(as.logical(calc_delta))

    if(reset_timer){
      last_time <<- NULL
    }

    logger::log_formatter(logger::formatter_paste, namespace = namespace)

    loglevel <- switch (
      level,
      'info' = logger::INFO,
      "warning" = logger::WARN,
      "error" = logger::ERROR,
      "fatal" = logger::FATAL,
      "debug" = logger::DEBUG,
      {
        logger::TRACE
      }
    )

    logger::log_layout(layout = rave_logger_layout, namespace = namespace)

    if(use_glue) {
      msg <- raveio::glue(..., .envir = .envir, .sep = .sep)
    } else {
      msg <- paste(..., collapse = .sep, sep = .sep)
    }

    logger::log_level(level = loglevel, namespace = namespace, msg)

    invisible(msg)

  }
})

#' @rdname logger
#' @export
logger_threshold <- function(
  level = c("info", "warning", "error", "fatal", "debug", "trace"),
  module_id, ...){

  level <- match.arg(level)

  if(missing(module_id) || !length(module_id)){
    module <- get_active_module_info()
    if(is.list(module)){
      namespace <- module$id
    } else {
      namespace <- "ravedash"
    }
  } else {
    namespace <- module_id
  }

  loglevel <- switch (
    level,
    'info' = logger::INFO,
    "warning" = logger::WARN,
    "error" = logger::ERROR,
    "fatal" = logger::FATAL,
    "debug" = logger::DEBUG,
    {
      logger::TRACE
    }
  )
  logger::log_threshold(level = loglevel, namespace = namespace, ...)
}
