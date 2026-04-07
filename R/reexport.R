# Compatible/obsolete

#' @export
dipsaus::`%OF%`

#' @importFrom dipsaus shiny_alert2
#' @export
dipsaus::shiny_alert2

#' @importFrom dipsaus close_alert2
#' @export
dipsaus::close_alert2

#' @importFrom shidashi show_notification
#' @export
shidashi::show_notification

#' @importFrom shidashi clear_notifications
#' @export
shidashi::clear_notifications

#' @importFrom shidashi register_output
#' @export
shidashi::register_output

#' @importFrom shidashi card_badge
#' @export
shidashi::card_badge

#' @importFrom shidashi card_recalculate_badge
#' @export
shidashi::card_recalculate_badge

#' @importFrom shidashi enable_recalculate_badge
#' @export
shidashi::enable_recalculate_badge

#' @importFrom shidashi disable_recalculate_badge
#' @export
shidashi::disable_recalculate_badge

#' @importFrom shidashi set_card_badge
#' @export
shidashi::set_card_badge

#' @importFrom ravepipeline logger
#' @export
ravepipeline::logger

#' @importFrom ravepipeline set_logger_path
#' @export
ravepipeline::set_logger_path

#' @importFrom ravepipeline logger_threshold
#' @export
ravepipeline::logger_threshold

#' @importFrom ravepipeline logger_error_condition
#' @export
ravepipeline::logger_error_condition

#' @name obsolete
#' @title Obsolete functions
#' @description
#' Reserved for compatibility; do not use.
#'
#' @param expr expression to evaluate
#' @param ... ignored
#' @param quoted whether \code{expr} is quoted
#' @param env environment to evaluate \code{expr}
#' @export
output_gadget_container <- function(expr, ..., quoted = FALSE, env = parent.frame()) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  eval(expr, envir = env)
}
