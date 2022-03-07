
safe_wrap_expr <- function(expr, onFailure = NULL, onError = NULL, finally = {}){
  expr_ = substitute(expr)

  tryCatch({
    force(expr)
  }, error = function(e){
    if(is.function(onFailure)){
      onFailure(e)
    }

    logger_error_condition(e)
    logger(c("Wrapped expressions:", deparse(expr_)), .sep = "\n", level = "error")
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


  }, finally = finally)
}

observe <- function(x, env = NULL, quoted = FALSE, priority = 0L, domain = NULL, ...){
  if(!quoted){
    x = substitute(x)
  }

  # Make sure shiny doesn't crash
  x = bquote({
    asNamespace('ravedash')$safe_wrap_expr(.(x))
  })

  if(!is.environment(env)){
    env = parent.frame()
  }
  if(is.null(domain)){
    domain = shiny::getDefaultReactiveDomain()
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

#' @export
safe_observe <- observe

