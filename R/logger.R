#' @name with_error_notification
#' @title Evaluate with automatic error handlers in dashboard
#' @description Keep track of messages printed by modules
#' @param ... passed to other methods
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
#' @return The condition \code{cond} if errored out, or the evaluated results
#'
NULL


#' @rdname with_error_notification
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

#' @rdname with_error_notification
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

#' @rdname with_error_notification
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

#' @rdname with_error_notification
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

  # wrap job
  workdir <- normalizePath(tempfile(pattern = "ravetmplog_"), mustWork = FALSE)
  ravepipeline::dir_create2(workdir)
  logfile <- file.path(workdir, "stdout.log")

  fun <- function() {}
  environment(fun) <- new.env(parent = parent.frame())
  body(fun) <- expr
  env <- new.env(parent = emptyenv())
  env$job_id <- ravepipeline::start_job(
    fun = fun,
    log_path = "stdout.log",
    workdir = workdir,
    method = "callr",
    ensure_init = TRUE,
    name = title,
    ...
  )
  env$promise <- ravepipeline::as.promise(env$job_id)

  task <- shiny::ExtendedTask$new(function() {
    env$promise
  })

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
              id = ns("@verbatim_log"),
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

  dismiss_handler <- shiny::bindEvent(
    safe_observe({
      shiny::removeModal(session = session)
    }, domain = session),
    session$input[["@dismiss_modal"]],
    once = TRUE,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  local_reactives <- shiny::reactiveValues()
  session$output[["@verbatim_log"]] <- shiny::renderPrint({
    cat(local_reactives$msg, sep = "\n")
  })

  renderMsg <- function(msg) {
    msg <- paste(msg, collapse = "\n")
    local_reactives$msg <- msg
    session$sendCustomMessage(
      "shidashi.set_html",
      list(
        selector = sprintf("code#%s", ns("@verbatim_log")),
        content = msg
      )
    )
  }

  shiny::bindEvent(
    shiny::observe({
      status <- task$status()
      if (!isTRUE(status %in% c("success", "error"))) { return() }
      dipsaus::updateActionButtonStyled(
        session = session, inputId = "@dismiss_modal",
        disabled = FALSE, label = "Dismiss")

      if (is.function(callback)) {
        result <- task$result()
        callback(result)
      }
    }, domain = session),
    task$result(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  logger("Initalizing job: [{ title }]", level = "trace", use_glue = TRUE)
  renderMsg("Initializing...")

  task$invoke()

  check <- function() {
    status <- tryCatch(
      {
        shiny::isolate(task$status())
      },
      error = function(e) {
        "success"
      }
    )

    if (file.exists(logfile)) {
      log <- readLines(logfile, warn = FALSE)
    } else {
      log <- "Waiting for outputs..."
    }

    if (isTRUE(status %in% c("success", "error"))) {
      unlink(logfile)
      log <- c(log, sprintf("Task finished with status: %s", status))
      logger("Job done: [{ title }]", level = "trace", use_glue = TRUE)
      renderMsg(log)
    } else {
      renderMsg(log)
      later::later(check, 0.5)
    }

    return()

  }

  check()
  env$promise
}


