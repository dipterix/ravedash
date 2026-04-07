# Compatible/obsolete

#' @importFrom shidashi register_output
#' @export
shidashi::register_output

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
