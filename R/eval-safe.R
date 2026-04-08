


safe_wrap_expr <- function(expr, onFailure = NULL, finally = {}, log_error = "error") {

  # rlang::try_fetch({
  #   force(expr)
  # }, error = function(e) {
  #   expr_str <- deparse(expr_)
  #   e <- rlang::error_cnd(
  #     class = c("rave_eval_error", "rave_error"),
  #     message = paste(c("Unable to evaluate the following expression: ", expr_str), collapse = "\n"),
  #     rave_error = list(
  #       name = "(safe-wrapped expression)",
  #       message = c(
  #         "Error found in code. Please inform module writers to fix it (details have been printed in console):",
  #         e$message
  #       )
  #     ),
  #     parent = e
  #   )
  #   if (is.function(onFailure)) {
  #     onFailure(e)
  #   }
  #
  #   logger_error_condition(e)
  #   # logger(paste(c("Wrapped expressions:", deparse(expr_)), collapse = "\n"),
  #   #        .sep = "\n", level = log_error, use_glue = FALSE)
  #
  #   if (shiny_is_running()) {
  #     try({
  #       ravedash::error_notification(
  #         cond = e, title = "Coding Error", type = "danger",
  #         autohide = FALSE, class = "rave-notifications-coding-error"
  #       )
  #     }, silent = TRUE)
  #   }

  expr_ <- substitute(expr)

  parent_frame <- parent.frame()
  current_env <- getOption('rlang_trace_top_env', NULL)
  options(rlang_trace_top_env = parent_frame)
  on.exit({
    options(rlang_trace_top_env = current_env)
  })

  tryCatch({
    # force(expr)
    eval(expr_, envir = parent_frame)
  }, error = function(e) {
    if (is.function(onFailure)) {
      try({ onFailure(e) })
    }

    error_notification(
      cond = e,
      title = "Coding Error",
      class = "rave-notifications-coding-error",
      prefix = "Error found in code. Please inform module writers to fix it (details have been printed in console):"
    )
    logger(c("Wrapped expressions:", deparse(expr_)), .sep = "\n", level = log_error)


  }, finally = try({
    finally
  }))
}

observe <- function(x, env = NULL, quoted = FALSE, priority = 0L, domain = NULL, ...,
                    error_wrapper = c("none", "notification", "alert"),
                    watch_data = getOption("ravedash.auto_watch_data", FALSE)) {
  error_wrapper <- match.arg(error_wrapper)
  if (!quoted) {
    x <- substitute(x)
  }

  if (watch_data) {
    x <- bquote({
      if (!shiny::isolate(asNamespace("ravedash")$watch_data_loaded())) {
        ravepipeline::logger("Data not loaded...")
        return(invisible())
      }
      .(x)
    })
  }

  # Make sure shiny doesn't crash
  switch(
    error_wrapper,
    "none" = {
      x <- bquote({
        asNamespace("ravedash")$safe_wrap_expr(.(x))
      })
    },
    "notification" = {
      x <- bquote({
        asNamespace("ravedash")$safe_wrap_expr({
          asNamespace("ravedash")$with_error_notification(.(x))
        })
      })
    },
    "alert" = {
      x <- bquote({
        asNamespace("ravedash")$safe_wrap_expr({
          asNamespace("ravedash")$with_error_alert(.(x))
        })
      })
    }
  )

  if (!is.environment(env)) {
    env <- parent.frame()
  }

  if (is.null(domain)) {
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
#' @param error_wrapper handler when error is encountered, choices are
#' \code{'none'}, \code{'notification'} (see \code{\link{error_notification}}),
#' or \code{'alert'} (see \code{\link{error_alert}})
#' @param watch_data whether to invalidate only when
#' \code{\link{watch_data_loaded}} is \code{TRUE}
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

# standalone_viewer moved to R/deprecated.R
