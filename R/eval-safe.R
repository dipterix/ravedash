


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
  current_env <- getOption("rlang_trace_top_env", NULL)
  options(rlang_trace_top_env = parent_frame)
  on.exit({
    options(rlang_trace_top_env = current_env)
  })

  tryCatch(
    {
      # force(expr)
      eval(expr_, envir = parent_frame)
    },
    error = function(e) {
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
    },
    observer_early_terminate = function(e) {
      # Do nothing
      message <- trimws(paste(e$message, collapse = ""))
      level <- e$level %||% "debug"
      if (nzchar(message)) {
        logger(message, level = )
      }
    },
    finally = try({
      finally
    })
  )
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


#' Safe-wrapper of 'shiny' \code{\link[shiny]{reactive}} function
#' @description A reactive expression whose errors never reach its consumers:
#' an error is logged (and optionally shown) and the expression yields the last
#' value it produced, or \code{initial} before its first success. Validation
#' errors (\code{\link[shiny]{req}}, \code{\link[shiny]{validate}}) pass
#' through, so outputs still stop rendering as they do with
#' \code{\link[shiny]{reactive}}.
#' @details 'shiny' caches the error raised by a reactive expression and raises
#' it again to every consumer until the expression is invalidated; when the
#' consumer is an observer, or the expression is a trigger of
#' \code{\link[shiny]{bindEvent}}, the session is closed. The last good value
#' therefore lives in this wrapper, not in the reactive's cache.
#' @param x,env,quoted,label,domain,... passed to \code{\link[shiny]{reactive}}
#' @param initial value returned while no evaluation has succeeded yet
#' @param error_wrapper handler when error is encountered, choices are
#' \code{'none'} (log only), \code{'notification'} (see
#' \code{\link{error_notification}}), or \code{'alert'} (see
#' \code{\link{error_alert}})
#' @return 'shiny' reactive expression
#'
#' @examples
#'
#' values <- shiny::reactiveValues(A = 1)
#'
#' doubled <- safe_reactive({
#'   if (values$A < 0) { stop("A must be non-negative") }
#'   values$A * 2
#' })
#'
#' # In a running app, `doubled()` keeps its last value when `values$A` turns
#' # negative; the error goes to the log instead of closing the session
#'
#' @export
safe_reactive <- function(
    x, env = parent.frame(), quoted = FALSE, ..., label = NULL,
    domain = shiny::getDefaultReactiveDomain(), initial = NULL,
    error_wrapper = c("none", "notification", "alert")) {

  error_wrapper <- match.arg(error_wrapper)
  if (!quoted) {
    x <- substitute(x)
  }
  if (!is.environment(env)) {
    env <- parent.frame()
  }
  if (is.null(label)) {
    label <- sprintf(
      "safe_reactive(%s)",
      substr(paste(deparse(x), collapse = " "), 1L, 60L)
    )
  }
  func <- shiny::exprToFunction(x, env = env, quoted = TRUE)

  # Closure state: the previous good value. Shiny's own cache holds the error
  # once the expression fails, so it cannot be read back from there.
  last_value <- initial

  shiny::reactive({
    tryCatch(
      {
        # Reactive reads inside `func()` register on this reactive as usual
        value <- func()
        last_value <<- value
        value
      },
      error = function(e) {
        # `req()`/`validate()` stop a render on purpose: consumers handle these
        if (inherits(e, "shiny.silent.error")) {
          stop(e)
        }
        switch(
          error_wrapper,
          "notification" = {
            error_notification(
              cond = e,
              title = "Coding Error",
              class = "rave-notifications-coding-error",
              prefix = "Error found in code. Please inform module writers to fix it (details have been printed in console):"
            )
          },
          "alert" = {
            error_alert(cond = e, title = "Coding Error")
          },
          {
            logger_error_condition(e)
          }
        )
        last_value
      },
      observer_early_terminate = function(e) {
        # Same contract as `safe_wrap_expr()`: a deliberate early stop
        message <- trimws(paste(e$message, collapse = ""))
        level <- e$level
        if (!length(level)) { level <- "debug" }
        if (nzchar(message)) {
          logger(message, level = level)
        }
        last_value
      }
    )
  }, label = label, domain = domain, ...)
}
