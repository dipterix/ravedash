#' Default module server function
#' @description Common shiny server function to enable modules that requires
#' data loader panel.
#' @param input,output,session shiny input, output, and session object
#' @param check_data_loaded a function that takes zero to one argument and
#' must return either \code{TRUE} if data has been loaded or \code{FALSE}
#' if loader needs to be open to load data.
#' @param ... ignored
#' @return A list of server utility functions; see 'Examples' below.
#' @examples
#'
#' # Debug in non-reactive session: create fake session
#' fake_session <- shiny::MockShinySession$new()
#' fake_input <- fake_session$input
#' fake_output <- fake_session$output
#'
#' # register common-server function
#' module_server_common(input = fake_input,
#'                      output = fake_output,
#'                      session = fake_session)
#' server_tools <- get_default_handlers(fake_session)
#'
#' # Print each function to see the usage
#'
#' server_tools$auto_recalculate
#'
#' server_tools$run_analysis_onchange
#'
#' server_tools$run_analysis_flag
#'
#'
#' # 'RAVE' module server function
#' server <- function(input, output, session, ...){
#'
#'   pipeline_path <- "PATH to module pipeline"
#'
#'   module_server_common(
#'     input, output, session,
#'     function(first_time){
#'
#'       re <- tryCatch({
#'         # Try to read data from pipeline results
#'         repo <- raveio::pipeline_read(
#'           'repository',
#'           pipe_dir = pipeline_path
#'         )
#'
#'         # Fire event to update footer message
#'         ravedash::fire_rave_event('loader_message',
#'                                   "Data loaded")
#'
#'         # Return TRUE indicating data has been loaded
#'         TRUE
#'       }, error = function(e){
#'
#'         # Fire event to remove footer message
#'         ravedash::fire_rave_event('loader_message', NULL)
#'
#'         # Return FALSE indicating no data has been found
#'         FALSE
#'       })
#'     }
#'   )
#'
#' }
#'
#' @export
module_server_common <- function(input, output, session, check_data_loaded, ...){

  if(missing(session)){
    if(dipsaus::shiny_is_running()){
      logger("`module_server_common`: session must be provided in production!", level = "fatal")
      stop("`module_server_common`: session must be provided in production!")
    } else {
      logger("`module_server_common`: session must be provided in production. Using mock session to debug.", level = "warning")
    }
    session <- shiny::MockShinySession$new()
    input <- session$input
    output <- session$output
  }

  if(missing(check_data_loaded)) {
    logger("`module_server_common`: `check_data_loaded` is missing. Data loader is disabled", level = "debug")
    check_data_loaded <- function(){ return(TRUE) }
  }


  ravedash::register_rave_session(session = session)
  reactive_handlers <- session$userData$ravedash_reactive_handlers
  local_reactives <- shiny::reactiveValues(
    first_time = TRUE,
    auto_recalculate = 0L,
    renew_auto_recalculate = NULL,
    auto_recalculate_back_up = 0L
  )

  handler_on_data_changed <- shiny::observe({
    # Check whether is there any missing data for this module
    check_results <- FALSE
    tryCatch({

      shiny::withLogErrors({
        if(length(formals(check_data_loaded)) == 0){
          check_results <- isTRUE(check_data_loaded())
        } else {
          check_results <- isTRUE(check_data_loaded(shiny::isolate(local_reactives$first_time)))
        }

        local_reactives$first_time <- FALSE
        if( check_results ){
          ravedash::logger("Checking whether data has been loaded: YES")
        } else {
          ravedash::logger("Checking whether data has been loaded: NO")
        }
      })
    }, error = function(e){
      ravedash::logger("Found an error while checking data...", level = "warning")
      ravedash::logger(paste(e$message, sep = "\n", collapse = "\n"), level = "error")
      msg <- paste(utils::capture.output({
        if(length(e$call)){
          cat("Error in ", deparse1(e$call), ": ", sep = "")
        } else {
          cat("Error: ")
        }
        cat(e$message, "\nTraceback:\n")
        traceback(e)
      }), collapse = "\n")
      shidashi::show_notification(
        title = "Error found!", autohide = FALSE, close = TRUE, type = 'danger',
        message = shiny::div(
          "Found an error while trying to check this module. The debug message is displayed below.",
          shiny::hr(),
          shiny::pre(msg)
        )
      )
    })

    # print(check_results)
    local_reactives$check_results <- check_results
    if(isTRUE(check_results)){
      shidashi::clear_notifications()
      ravedash::logger("Skip loader interface", level = "debug")
      ravedash::close_loader()
      ravedash::fire_rave_event('data_loaded', Sys.time())
    } else {
      ravedash::logger("Opening loader interface")
      ravedash::open_loader()
      ravedash::fire_rave_event('data_loaded', FALSE)
    }
  }) |>
    shiny::bindEvent(
      ravedash::get_rave_event("data_changed"),
      ignoreInit = FALSE, ignoreNULL = FALSE
    )

  shiny::observe({
    toggle <- ravedash::get_rave_event("toggle_loader")
    # active_module <- ravedash::watch_active_module()
    if(!ravedash::watch_loader_opened()){
      ravedash::open_loader()
    } else if(isTRUE(local_reactives$check_results)){
      ravedash::close_loader()
    }

  }) |>
    shiny::bindEvent(ravedash::get_rave_event("toggle_loader"),
                     ignoreInit = FALSE, ignoreNULL = TRUE)

  output$loader_short_message <- shiny::renderText({
    msg <- paste(ravedash::get_rave_event('loader_message'), collapse = "")
    msg
  })

  handler_open_loader <- shiny::observe({
    # Listen to a global event on whether data has changed
    loader_open <- ravedash::watch_loader_opened()
    if(loader_open){
      local_reactives$open_loader <- Sys.time()
      shidashi::add_class(".module_main_ui", "soft-hidden")
      shidashi::remove_class(".module_loader_ui", "soft-hidden")
    } else {
      local_reactives$open_loader <- FALSE
      shidashi::add_class(".module_loader_ui", "soft-hidden")
      shidashi::remove_class(".module_main_ui", "soft-hidden")
    }
  }) |>
    shiny::bindEvent(
      ravedash::watch_loader_opened(),
      ignoreInit = FALSE,
      ignoreNULL = FALSE
    )


  run_analysis_flag <- shiny::debounce(shiny::reactive({
    if(shiny::isolate(isTRUE(
        local_reactives$auto_recalculate_back_up > 0 &&
        local_reactives$auto_recalculate <= 0
    ))) {
      auto_recalculate(local_reactives$auto_recalculate_back_up)
      return(NULL)
    }
    get_rave_event("run_analysis")
  }), millis = 300, priority = 99)

  run_analysis <- function() {

    module_id <- session$ns(NULL)
    if(!length(module_id) || module_id == ""){ return() }
    module <- shiny::isolate(get_rave_event("active_module"))
    if(!(is.list(module) && isTRUE(module$id == module_id))) {
      logger("Module ID: expected {module_id} vs. actual {module$id}",
                       level = "trace", use_glue = TRUE)
      return()
    }
    if(shiny::isolate(watch_loader_opened())) {
      logger("Module loader has been opened, pause auto-calculation", level = "trace")
      return()
    }

    fire_rave_event(
      key = "run_analysis",
      list(
        type = "run_analysis",
        timestamp = strftime(Sys.time(), "%Y-%m-%dT%T"),
        parent_frame = FALSE
      ),
      force = TRUE,
      global = FALSE
    )
  }

  sensitive_input <- shiny::reactive({
    shiny::reactiveValuesToList(input)
  })

  watch_input_changed <- shiny::debounce({
    shiny::bindEvent(
      shiny::reactive({
        watch_ids <- local_reactives$watch_ids
        if(!length(watch_ids)){ return(NULL) }
        inputs <- sensitive_input()
        res <- structure(lapply(watch_ids, function(nm){
          inputs[[nm]]
        }), names = watch_ids)
        dipsaus::drop_nulls(res)
      }),
      local_reactives$watch_ids,
      local_reactives$renew_auto_recalculate,
      sensitive_input(),
      ignoreNULL = TRUE, ignoreInit = TRUE
    )
  }, millis = 100, priority = 100)

  run_analysis_onchange <- function(inputIds){
    logger("run_analysis_onchange", level = "trace")
    local_reactives$watch_ids <- inputIds
  }

  shiny::observe({

    v <- local_reactives$auto_recalculate
    vb <- local_reactives$auto_recalculate_back_up
    if(v <= 0 && vb <= 0){ return() }

    if(v > 0){
      module_id <- session$ns(NULL)
      module <- shiny::isolate(get_rave_event("active_module"))
      if(
        !length(module_id) || module_id == "" ||
        !is.list(module) || !isTRUE(module$id == module_id)
      ){
        local_reactives$auto_recalculate_back_up <- local_reactives$auto_recalculate
        local_reactives$auto_recalculate <- 0L
        shidashi::show_notification(message = shiny::div(
          shiny::p("Auto re-calculation is temporarily disabled because you have switched to another module."),
          dipsaus::actionButtonStyled(session$ns("_reenable_autocalculation_"), "Click here to re-activate")
        ), title = "Auto re-calculation disabled", type = "info", close = TRUE, autohide = FALSE,
        class = "ravedash-reenable_autocalculation-notif")
        return()
      }

      local_reactives$auto_recalculate <- v - 1
      local_reactives$auto_recalculate_back_up <- 0L
      shidashi::clear_notifications(class = "ravedash-reenable_autocalculation-notif")
      logger("Auto-recalculation triggered", level = "trace")
      run_analysis()

    }

  }) |>
    shiny::bindEvent(
      watch_input_changed(),
      local_reactives$renew_auto_recalculate,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

  shiny::observe({
    local_reactives$auto_recalculate <- max(
      local_reactives$auto_recalculate,
      local_reactives$auto_recalculate_back_up
    )
    local_reactives$auto_recalculate_back_up <- 0L
    local_reactives$renew_auto_recalculate <- Sys.time()
    shidashi::clear_notifications(class = "ravedash-reenable_autocalculation-notif")
  }) |>
    shiny::bindEvent(
      input[["_reenable_autocalculation_"]]
    )

  auto_recalculate <- function(flag){

    current <- isTRUE(shiny::isolate(local_reactives$auto_recalculate > 0))
    if(missing(flag)){
      return(current)
    }
    if(length(flag) != 1){
      logger("`auto_recalculate`: flag must be length of 1", level = "warning")
      return(current)
    }
    if(is.logical(flag)){
      if(flag){
        local_reactives$auto_recalculate <- Inf
        local_reactives$auto_recalculate_back_up <- 0L
      } else {
        local_reactives$auto_recalculate <- 0L
        local_reactives$auto_recalculate_back_up <- 0L
      }
      local_reactives$renew_auto_recalculate <- Sys.time()
    } else {
      if(!is.numeric(flag)){
        logger("`auto_recalculate`: flag must be logical or numeric", level = "warning")
        return(current)
      }
      local_reactives$auto_recalculate <- flag
      local_reactives$auto_recalculate_back_up <- 0L
      local_reactives$renew_auto_recalculate <- Sys.time()
    }
    shidashi::clear_notifications(class = "ravedash-reenable_autocalculation-notif")
    logger("Set auto-recalculate flag to {flag}", level = "trace", use_glue = TRUE)
    return(isTRUE(flag > 0))
  }

  reactive_handlers$run_analysis_flag <- structure(
    run_analysis_flag, class = "ravedash_printable",
    docs = paste(
      sep = "\n",
      "Flag to run analysis pipeline. Usage:\n",
      "# Obtain the server utility functions",
      "module_server_common(input, output, session)",
      "server_tools <- get_default_handlers()\n",
      "shiny::bindEvent(",
      "  shiny::observe({",
      "    <Run your algorithms, e.g. `raveio::pipeline_run(...)>",
      "  }),",
      "  server_tools$run_analysis_flag(),",
      "  ignoreNULL = TRUE, ignoreInit = TRUE",
      ")"
    )
  )
  # reactive_handlers$run_analysis <- run_analysis
  reactive_handlers$run_analysis_onchange <- structure(
    run_analysis_onchange, class = "ravedash_printable",
    docs = paste(
      sep = "\n",
      "Function to set input IDs to watch. These inputs will trigger auto re-calculate. Usage:\n",
      "# Obtain the server utility functions",
      "module_server_common(input, output, session)",
      "server_tools <- get_default_handlers()\n",
      'server_tools$run_analysis_onchange(c("inputId_1", "inputId_2", ...))'
    )
  )
  reactive_handlers$auto_recalculate <- structure(
    auto_recalculate, class = "ravedash_printable",
    docs = paste(
      sep = "\n",
      "Function to turn auto-recalculation on and off. Usage:\n",
      "# Obtain the server utility functions",
      "module_server_common(input, output, session)",
      "server_tools <- get_default_handlers()\n",
      'server_tools$auto_recalculate(TRUE)    # Turn on forever',
      'server_tools$auto_recalculate(FALSE)   # Turn off forever',
      'server_tools$auto_recalculate(1)       # Turn on once'
    )
  )

  invisible(reactive_handlers)
}

#' @export
print.ravedash_printable <- function(x, ...){
  docs <- attr(x, "docs")
  if(is.null(docs)){
    NextMethod("print")
  } else {
    cat(attr(x, "docs"), "\n", sep = "")
  }
}
