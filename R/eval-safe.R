
safe_wrap_expr <- function(expr, onFailure = NULL, onError = NULL, finally = {}, log_error = "error"){
  expr_ <- substitute(expr)

  parent_frame <- parent.frame()
  options(rlang_trace_top_env = parent_frame)
  current_env <- getOption('rlang_trace_top_env', NULL)
  on.exit({
    options(rlang_trace_top_env = current_env)
  })

  tryCatch({
    force(expr)
  }, error = function(e){
    if(is.function(onFailure)){
      onFailure(e)
    }

    if(dipsaus::shiny_is_running()) {
      try({
        shidashi::show_notification(
          title = "Coding Error", type = "danger",
          autohide = FALSE, collapse = "\n", class = "rave-notifications-coding-error",
          message = c(
            "Error found in code. Please inform module writers to fix it (details have been printed in console):",
            e$message
          )
        )
      }, silent = TRUE)
    }

    logger_error_condition(e)
    logger(c("Wrapped expressions:", deparse(expr_)), .sep = "\n", level = log_error)


  }, finally = finally)
}

observe <- function(x, env = NULL, quoted = FALSE, priority = 0L, domain = NULL, ...){
  if(!quoted){
    x <- substitute(x)
  }

  # Make sure shiny doesn't crash
  x <- bquote({
    asNamespace('ravedash')$safe_wrap_expr(.(x))
  })

  if(!is.environment(env)){
    env <- parent.frame()
  }
  if(is.null(domain)){
    domain <- shiny::getDefaultReactiveDomain()
  }
  shiny::observe(
    x = x,
    env = env,
    quoted = TRUE,
    priority = priority,
    domain = domain,
    ...
  )
}

#' Safe-wrapper of 'shiny' \code{\link[shiny]{observe}} function
#' @description Safely wrap expression \code{x} such that shiny application does
#' no hang when when the expression raises error.
#' @param x,env,quoted,priority,domain,... passed to \code{\link[shiny]{observe}}
#' @return 'shiny' observer instance
#'
#' @examples
#'
#' values <- shiny::reactiveValues(A=1)
#'
#' obsB <- safe_observe({
#'   print(values$A + 1)
#' })
#'
#' @export
safe_observe <- observe

