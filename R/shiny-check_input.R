#' Check shiny inputs and modify if validation fails
#' @param inputId character, input ID
#' @param check either a function that takes the input value or a character
#' of a \code{checkmate} function; when \code{check} is a character, this
#' function will look for \code{check_*} functions in the \code{checkmate}
#' package
#' @param ... passed to \code{check} function
#' @param on_check_fails value to substitute when check fails, and the input
#' value will be the result of \code{on_check_fails}. This argument can be
#' missing; when missing, input value will not be altered
#' @param quoted whether \code{on_check_fails} is quoted
#' @param env environment to evaluate \code{on_check_fails}
#' @param logger_level log level when validation fails
#' @param session shiny session; default is current session
#' @returns A shiny observe instance
#' @examples
#'
#' if(interactive()) {
#'
#' library(ravedash)
#' shiny::shinyApp(
#'   ui = shiny::basicPage(
#'     shiny::textInput("id1", "Enter a text"),
#'     shiny::textOutput("id2")
#'   ),
#'   server = function(input, output, session) {
#'     # input$id1 must have at least 1 character
#'     # the check uses `checkmate::check_character`
#'     shiny_check_input(
#'       "id1",
#'       check = "character",
#'       min.chars = 1,
#'       on_check_fails = "altered text"
#'     )
#'
#'     output$id2 <- shiny::renderText({
#'       print(input$id1)
#'       sprintf("The final value is: %s", input$id1)
#'     })
#'   }
#' )
#'
#' }
#'
#' @export
shiny_check_input <- function(
  inputId,
  check = NULL,
  on_check_fails,
  ...,
  quoted = FALSE,
  env = parent.frame(),
  logger_level = c("trace", "none", "debug", "info", "warning", "error"),
  session = shiny::getDefaultReactiveDomain()
) {
  logger_level <- match.arg(logger_level)
  parent_frame <- parent.frame()

  if (length(check)) {
    if (length(check) != 1) {
      stop("shiny_check_input: `check` must be NULL or length of 1")
    }
    if (is.character(check)) {
      checkmate <- asNamespace("checkmate")
      check <- checkmate[[sprintf("check_%s", check)]]
      if (!is.function(check)) {
        stop(
          "shiny_check_input: `check` is invalid, ",
          "must be a function or a (*) in checkmate::check_*"
        )
      }
    }
  }

  if (!is.function(check)) {
    check <- function(...) {
      TRUE
    }
  }

  call <- match.call(expand.dots = FALSE)
  if ("..." %in% names(as.list(call))) {
    dots <- call[["..."]]
  } else {
    dots <- list()
  }

  check_function <- function(value) {
    check_call <- as.call(c(list(check, value), dots))

    return(tryCatch(
      {
        eval(check_call, parent_frame)
      },
      error = function(e) {
        e$message
      }
    ))
  }

  on_check_fails_missing <- missing(on_check_fails)
  if (on_check_fails_missing) {
    on_check_fails <- NULL
  } else if (!quoted) {
    on_check_fails <- substitute(on_check_fails)
  }

  auto_correcting <- FALSE
  last_x <- structure(list(), class = "key_missing")

  auto_correct <- function(x) {
    if (auto_correcting && identical(x, last_x)) {
      return(x)
    }
    check_result <- check_function(x)
    if (isTRUE(check_result)) {
      auto_correcting <<- FALSE
      return(x)
    }
    if (!on_check_fails_missing) {
      x <- eval(on_check_fails, envir = env)
    }
    if (logger_level != "none") {
      if (on_check_fails_missing) {
        logger(
          level = logger_level,
          use_glue = FALSE,
          sprintf(
            "Input [%s] fails the check: %s... (input value not altered)",
            inputId,
            paste(check_result, collapse = "")
          )
        )
      } else {
        logger(
          level = logger_level,
          use_glue = FALSE,
          sprintf(
            "Input [%s] fails the check: %s... Setting input value to an alternative value: %s", # nolint: line_length_linter.
            inputId,
            paste(check_result, collapse = ""),
            deparse1(x)
          )
        )
      }
    }
    if (on_check_fails_missing) {
      auto_correcting <<- FALSE
      return(x)
    }
    auto_correcting <<- TRUE
    x <- eval(on_check_fails, envir = env)
    impl <- .subset2(session$input, "impl")
    impl$set(inputId, value = x)
    return(x)
  }

  ravedash::safe_observe(
    {
      value <- session$input[[inputId]]
      auto_correct(value)
    },
    priority = 10001
  )
}
