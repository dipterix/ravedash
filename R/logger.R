#' @name logger
#' @title Logger system used by 'RAVE'
#' @description Keep track of messages printed by modules
#' @param ...,.envir,.sep passed to \code{glue}, if
#' \code{use_glue} is true
#' @param use_glue whether to use \code{glue} to combine
#' \code{...}; default is false
#' @param calc_delta whether to calculate time difference between current
#' message and previous message; default is \code{'auto'}, which prints
#' time difference when \code{level} is \code{'debug'}.
#' This behavior can be changed by altering \code{calc_delta} by a logical
#' \code{TRUE} to enable or \code{FALSE} to disable.
#' @param level the level of message, choices are \code{'info'} (default),
#' \code{'success'}, \code{'warning'}, \code{'error'}, \code{'fatal'},
#' \code{'debug'}, \code{'trace'}
#' @param module_id 'RAVE' module identification string, or name-space; default
#' is \code{'ravedash'}
#' @param reset_timer whether to reset timer used by \code{calc_delta}
#' @param root_path root directory if you want log messages to be saved to
#' hard disks; if \code{root_path} is \code{NULL}, \code{""}, or
#' \code{\link{nullfile}}, then logger path will be unset.
#' @param max_bytes maximum file size for each logger partitions
#' @param max_files maximum number of partition files to hold the log; old
#' files will be deleted.
#' @param type which type of logging should be set; default is \code{'console'},
#' if file log is enabled through \code{set_logger_path}, \code{type} could be
#' \code{'file'} or \code{'both'}. Default log level is \code{'info'} on
#' console and \code{'debug'} on file.
#' @param cond condition to log
#' @param class,title,delay,autohide passed to \code{\link[shidashi]{show_notification}}
#' @param session shiny session
#' @param expr expression to evaluate
#' @param envir environment to evaluate \code{expr}
#' @param quoted whether \code{expr} is quoted; default is false
#' @param collapse,danger_mode,auto_close,buttons will be passed to
#' \code{\link[dipsaus]{shiny_alert2}} or
#' \code{\link[shidashi]{show_notification}}
#' @param prefix additional messages to display in the notification or alert
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
NULL


#' @rdname logger
#' @export
error_notification <- function(
  cond,
  title = "Error found!",
  type = "danger",
  class = "error_notif",
  delay = 30000,
  autohide = TRUE,
  collapse = "\n",
  prefix = paste(
    "Found the following error",
    "(details have been printed in the console):"
  ),
  session = shiny::getDefaultReactiveDomain()
) {
  try({
    cond <- logger_error_condition(cond = cond)
    if (!is.null(session) && !inherits(cond, "rave_muffled")) {
      shidashi::show_notification(
        session = session,
        message = paste0(
          paste(prefix, collapse = ""),
          "\n\n",
          paste(cond$message, collapse = "\n")
        ),
        title = title,
        type = type,
        close = TRUE,
        autohide = autohide,
        delay = delay,
        class = session$ns(class),
        collapse = "\n"
      )
    }
  })
  invisible(cond)
}

#' @rdname logger
#' @export
error_alert <- function(
  cond,
  title = "Error found!",
  type = "error",
  danger_mode = TRUE,
  auto_close = FALSE,
  prefix = paste(
    "Found the following error",
    "(details have been printed in the console):"
  ),
  buttons = "Confirm",
  session = shiny::getDefaultReactiveDomain()
) {
  try({
    cond <- logger_error_condition(cond = cond)
    if (!is.null(session) && !inherits(cond, "rave_muffled")) {
      dipsaus::close_alert2()
      dipsaus::shiny_alert2(
        title = title,
        text = paste0(
          paste(prefix, collapse = ""),
          "\n\n",
          paste(cond$message, collapse = "\n")
        ),
        icon = type,
        danger_mode = danger_mode,
        auto_close = auto_close,
        session = session,
        buttons = buttons
      )
    }
  })
  invisible(cond)
}

#' @rdname logger
#' @export
with_error_notification <- function(
  expr, envir = parent.frame(), quoted = FALSE, ...
) {
  if (!quoted) {
    expr <- substitute(expr)
  }

  args <- list(...)

  tryCatch(
    {
      force(envir)
      eval(expr, envir = envir)
    },
    error = function(e) {
      logger(
        sprintf("Wrapped expression:\n%s", deparse1(expr, collapse = "\n")),
        level = "error",
        use_glue = FALSE
      )
      args$cond <- e
      do.call(error_notification, args)
    }
  )
}

#' @rdname logger
#' @export
with_error_alert <- function(
  expr,
  envir = parent.frame(),
  quoted = FALSE,
  ...
) {
  if (!quoted) {
    expr <- substitute(expr)
  }

  args <- list(...)

  tryCatch(
    {
      force(envir)
      eval(expr, envir = envir)
    },
    error = function(e) {
      logger(
        sprintf("Wrapped expression:\n%s", deparse1(expr, collapse = "\n")),
        level = "error",
        use_glue = FALSE
      )
      args$cond <- e
      do.call(error_alert, args)
    }
  )
}


#' @title Evaluate script in the background and show the results from
#' shiny modal dialogue
#' @param expr R expression to evaluate The script must be standalone
#' @param quoted whether the expression has been quoted
#' @param callback callback function to run once the evaluate finishes; must
#' take one argument. The passed variable will be the evaluation results or
#' an error condition (if error occurs)
#' @param title,size modal title and size, see \code{\link[shiny]{showModal}}
#' @param session shiny session object
#' @param ... ignored, reserved for future use
#' @returns A promise object
#'
#' @examples
#'
#' # Shiny server function
#' server <- function(input, output, session) {
#'   shiny::bindEvent(
#'     shiny::observe({
#'       with_log_modal(
#'         title = "Roll the dice",
#'         expr = {
#'           for(i in 1:10) {
#'             Sys.sleep(runif(1, min = 0.5, max = 2))
#'             cat(sprintf("Rolling dice result: %.0f\n", sample(6, 1)))
#'           }
#'         }
#'       )
#'       return()
#'     }),
#'     input$btn,
#'     ignoreNULL = TRUE, ignoreInit = TRUE
#'   )
#' }
#'
#' if(interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::basicPage(
#'       shiny::actionButton('btn', "Click me")
#'     ),
#'     server = server,
#'     options = list(launch.browser = TRUE)
#'   )
#' }
#'
#'
#' @export
with_log_modal <- function(
    expr, quoted = FALSE, callback = NULL, title = "Running...", size = "l",
    session = shiny::getDefaultReactiveDomain(), ...) {

  ns <- session$ns
  if (!quoted) {
    expr <- substitute(expr)
  }

  shiny::showModal(
    shiny::modalDialog(
      title = title,
      size = size,
      easyClose = FALSE,
      shidashi::flex_container(
        class = "fill-width max-height-500 overflow-y-auto",
        style = "flex-direction: column-reverse; max-width: 100%; max-height: 500px; overflow: auto",
        shidashi::flex_item(
          shiny::tags$pre(
            .noWS = c("outside", "after-begin", "before-end"),
            shiny::tags$code(
              id = ns("verbatim_log___"),
              class = "shiny-text-output hljs-literal",
              style = "word-wrap:break-word;width: 100%;white-space: pre-wrap;",
              .noWS = c("outside", "after-begin", "before-end")
            )
          )
        )
      ),
      footer = dipsaus::actionButtonStyled(
        ns("@dismiss_modal"),
        "Running...",
        disabled = ""
      )
    ),
    session = session
  )

  shiny::bindEvent(
    safe_observe({
      shiny::removeModal(session = session)
    }),
    session$input[["@dismiss_modal"]],
    once = TRUE,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  local_reactives <- shiny::reactiveValues()
  session$output[["verbatim_log___"]] <- shiny::renderPrint({
    cat(local_reactives$msg, sep = "\n")
  })

  renderMsg <- function(msg) {
    msg <- paste(msg, collapse = "\n")
    local_reactives$msg <- msg
    session$sendCustomMessage(
      "shidashi.set_html",
      list(
        selector = sprintf("code#%s", ns("verbatim_log___")),
        content = msg
      )
    )
  }

  logfile <- normalizePath(tempfile(pattern = "ravetmplog_"), mustWork = FALSE)
  logfile2 <- paste0(logfile, ".bak")

  check <- dipsaus::rs_exec(
    expr = expr,
    quoted = TRUE,
    as_promise = FALSE,
    focus_on_console = TRUE,
    nested_ok = TRUE,
    name = title,
    rs = FALSE,
    wait = FALSE,
    args = "--no-save --no-restore --slave",
    stdout = logfile,
    stderr = logfile
  )

  logger("Initalizing job: [{ title }]", level = "trace", use_glue = TRUE)
  renderMsg("Initializing...")


  final <- function(result, has_error = FALSE) {

    dipsaus::updateActionButtonStyled(
      session = session, inputId = "@dismiss_modal",
      disabled = FALSE, label = "Dismiss")

    unlink(logfile)
    unlink(logfile2)

    logger("Job done: [{ title }]", level = "trace", use_glue = TRUE)

    if (is.function(callback)) {
      callback( result )
    }
  }

  promise_f <- get_function_from("promise", "promises")
  then_f <- get_function_from("then", "promises")

  promise <- promise_f(function(resolve, reject) {

    later <- get_function_from("later", "later")

    listener <- function() {
      if (is.function(check)) {
        code <- check()
      } else {
        code <- check
      }
      if (
        length(logfile) != 1 ||
          is.na(logfile) ||
          !file.exists(logfile) ||
          logfile == ""
      ) {
        msg <- NULL
      } else {
        file.copy(logfile, logfile2, overwrite = TRUE)
        suppressWarnings({
          msg <- readLines(logfile2)
        })
        if (!length(msg) || isTRUE(msg == "")) {
          msg <- "Waiting for outputs..."
        }
      }

      if (code == 0) {
        renderMsg(c(msg, "Finished."))
        resolve(attr(code, "rs_exec_result"))
      } else if (code < 0) {
        renderMsg(c(msg, "An error is detected."))
        reject(attr(code, "rs_exec_error"))
      } else {
        renderMsg(msg)
        later(listener, delay = 0.5)
      }
    }
    listener()
  })

  then_f(
    promise,
    onFulfilled = function(result) {
      final(result = result, has_error = FALSE)
    },
    onRejected = function(e) {
      final(result = e, has_error = TRUE)
    }
  )
}


