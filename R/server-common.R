#' Default module server function
#' @description Common shiny server function to enable modules that requires
#' data loader panel.
#' @param module_id 'RAVE' module ID
#' @param session shiny session
#' @param check_data_loaded a function that takes zero to one argument and
#' must return either \code{TRUE} if data has been loaded or \code{FALSE}
#' if loader needs to be open to load data.
#' @param parse_env environment used to parse module
#' @param ... ignored
#' @return A list of server utility functions; see 'Examples' below.
#' @examples
#'
#' # Debug in non-reactive session: create fake session
#' fake_session <- shiny::MockShinySession$new()
#'
#' # register common-server function
#' module_server_common(module_id = "mock-session",
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
#' server_tools$module_is_active
#'
#' server_tools$simplify_view
#'
#' # 'RAVE' module server function
#' server <- function(input, output, session, ...){
#'
#'   pipeline_path <- "PATH to module pipeline"
#'
#'   module_server_common(
#'     module_id = session$ns(NULL),
#'     check_data_loaded = function(first_time){
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
#'     }, session = session
#'   )
#'
#' }
#'
#' @export
module_server_common <- function(module_id, check_data_loaded, ..., session = shiny::getDefaultReactiveDomain(), parse_env = NULL){

  if(!length(session)){
    if(dipsaus::shiny_is_running()){
      logger("`module_server_common`: session must be provided in production!", level = "fatal")
      stop("`module_server_common`: session must be provided in production!")
    } else {
      logger("`module_server_common`: session must be provided in production. Using mock session to debug.", level = "warning")
      root_session <- shiny::MockShinySession$new()
      session <- session$makeScope(module_id)
      input <- session$input
      output <- session$output
    }
  } else {
    root_session <- session$rootScope()
    if(!identical(session$ns(NULL), module_id)){
      logger("`module_server_common`: session scope is inconsistent (expected: {module_id}, actual: {session$ns(NULL)}).", level = "warning", use_glue = TRUE)
      if(dipsaus::shiny_is_running()){
        stop("Inconsistent `module_id`. see the warnings above.")
      }
    }
    input <- session$input
    output <- session$output
  }

  if(missing(check_data_loaded) || !is.function(check_data_loaded)) {
    logger("`module_server_common`: `check_data_loaded` is missing. Data loader is disabled", level = "debug")
    check_data_loaded <- function(){ return(TRUE) }
  }


  tools <- ravedash::register_rave_session(session = session)
  reactive_handlers <- session$userData$ravedash_reactive_handlers
  local_reactives <- shiny::reactiveValues(
    first_time = TRUE,
    auto_recalculate = 0L,
    renew_auto_recalculate = NULL,
    auto_recalculate_back_up = 0L,
    view_simplified = FALSE,
    watch_input_changed = NULL,
    rave_action = list(
      type = "active_module",
      id = module_id,
      parent_frame = FALSE
    )
  )
  local_data <- dipsaus::fastmap2()


  module_is_active <- function(id = module_id){
    module <- get_active_module_info()
    if(!is.list(module) || length(module$id) != 1){ return(FALSE) }
    identical(module$id, id)
  }


  # parse, load module
  observe({
    rave_action <- root_session$input[["@rave_action@"]]

    if(!is.list(rave_action) || !length(rave_action$type)) { return() }

    if(!identical(rave_action$type, "active_module") && !shiny::isolate(module_is_active())){ return() }

    parent_frame <- FALSE
    if(!isFALSE(rave_action$parent_frame)){
      parent_frame <- TRUE

      if(!module_is_active(rave_action$`_active_module`)){ return() }

    }
    rave_action$parent_frame <- parent_frame

    local_reactives$rave_action <- rave_action
  }, priority = 100001)

  get_rave_action <- shiny::debounce(shiny::reactive({
    local_reactives$rave_action
  }), millis = 50)

  shiny::bindEvent(
    observe({
      rave_action <- get_rave_action()
      fire_rave_event(key = rave_action$type, value = rave_action, .internal_ok = TRUE)
      if(rave_action$parent_frame) {
        ravedash::logger("[{rave_action$type}] A JavaScript rave-action fired from parent frame.",
                         level = "trace", use_glue = TRUE)
      } else {
        ravedash::logger("[{rave_action$type}] A JavaScript rave-action detected from local frame.",
                         level = "trace", use_glue = TRUE)
      }
    }),
    get_rave_action(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )


  handler_on_data_changed <- shiny::bindEvent(
    observe({
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
    }),
    ravedash::get_rave_event("data_changed"),
    ignoreInit = FALSE, ignoreNULL = FALSE
  )

  shiny::bindEvent(
    observe({
      toggle <- ravedash::get_rave_event("toggle_loader")
      # active_module <- ravedash::watch_active_module()
      if(!ravedash::watch_loader_opened()){
        ravedash::open_loader()
      } else if(isTRUE(local_reactives$check_results)){
        ravedash::close_loader()
      }

    }),
    ravedash::get_rave_event("toggle_loader"),
    ignoreInit = FALSE, ignoreNULL = TRUE
  )

  output[["__loader_short_message__"]] <- shiny::renderText({
    msg <- trimws(paste(ravedash::get_rave_event('loader_message'), collapse = ""))
    if(msg == "") {
      msg <- "Toggle data selector"
    }
    msg
  })

  output[["__recalculation_message__"]] <- shiny::renderText({
    if(isTRUE(local_reactives$auto_recalculate > 0)) {
      return("ON")
    } else {
      return("OFF")
    }
  })

  handler_open_loader <- shiny::bindEvent(
    observe({
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
    }),
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
    logger("Triggering a deferred signal to run_analysis_flag()", level = "trace")
    get_rave_event("run_analysis")
  }), millis = 300, priority = 99)

  shiny::bindEvent(
    observe({
      v <- !isTRUE(local_reactives$auto_recalculate > 0)
      auto_recalculate(v)
      msg <- sprintf("Auto re-calculation is turned %s", ifelse(v, "ON", "OFF"))
      shidashi::show_notification(
        message = msg, title = "Info", type = 'success',
        close = TRUE, autohide = TRUE
      )
    }),
    get_rave_event("toggle_auto_recalculation"),
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  shiny::bindEvent(
    observe({
      logger("run_analysis_flag() triggered!", level = "trace")
      local_data$changed_inputs <- NULL
    }),
    run_analysis_flag(),
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

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

  shiny::bindEvent(
    observe({
      watch_ids <- local_reactives$watch_ids
      if(!length(watch_ids)){ return(NULL) }
      inputs <- sensitive_input()
      res <- structure(lapply(watch_ids, function(nm){
        inputs[[nm]]
      }), names = watch_ids)

      if(!is.null(local_data$previous_input)) {
        changed <- lapply(watch_ids, function(nm){
          actual <- res[[nm]]
          expected <- local_data$previous_input[[nm]]
          if(!identical(actual, expected)){
            return(nm)
          }
          return()
        })
        changed <- unlist(changed)
      } else {
        changed <- watch_ids
      }

      local_data$previous_input <- res

      if(length(changed)){
        changed <- changed[!changed %in% local_data$changed_inputs]
        local_data$changed_inputs <- c(local_data$changed_inputs, changed)

        if(length(changed)){
          changed <- paste(changed, collapse = ", ")
          logger("Detected input change - [{changed}]",
                 level = "trace", use_glue = TRUE)
        }
        local_reactives$watch_input_changed <- Sys.time()
      }
    }),
    local_reactives$watch_ids,
    local_reactives$renew_auto_recalculate,
    sensitive_input(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  watch_input_changed <- shiny::debounce(shiny::reactive({
    local_reactives$watch_input_changed
  }), millis = 100, priority = 100)

  run_analysis_onchange <- function(inputIds){
    logger("run_analysis_onchange", level = "trace")
    local_reactives$watch_ids <- inputIds
  }

  shiny::bindEvent(
    observe({

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

    }),
    watch_input_changed(),
    local_reactives$renew_auto_recalculate,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  shiny::bindEvent(
    observe({
      local_reactives$auto_recalculate <- max(
        local_reactives$auto_recalculate,
        local_reactives$auto_recalculate_back_up
      )
      local_reactives$auto_recalculate_back_up <- 0L
      local_reactives$renew_auto_recalculate <- Sys.time()
      shidashi::clear_notifications(class = "ravedash-reenable_autocalculation-notif")
    }),
    input[["_reenable_autocalculation_"]],
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    observe({
      simplified <- shiny::isolate(!isTRUE(local_reactives$view_simplified))
      local_reactives$view_simplified <- simplified
      if(simplified){
        shidashi::add_class(".rave-optional", "soft-hidden")
      } else {
        shidashi::remove_class(".rave-optional", "soft-hidden")
      }

    }),
    ravedash::get_rave_event("simplify_toggle"),
    ignoreInit = FALSE, ignoreNULL = TRUE
  )

  simplify_view <- function(action = c("toggle", "yes", "no")) {
    action <- match.arg(action)
    if(action == "yes"){
      local_reactives$view_simplified <- FALSE
    } else if (action == "no") {
      local_reactives$view_simplified <- TRUE
    }
    ravedash::fire_rave_event("simplify_toggle", value = Sys.time(),
                              force = TRUE, global = FALSE, session = session)
  }

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


  reactive_handlers$simplify_view <- structure(
    simplify_view, class = "ravedash_printable",
    docs = paste(
      sep = "\n",
      "Show, hide, or toggle display of elements with `rave-optional` HTML class. Usage:\n",
      "# Obtain the server utility functions",
      "server_tools <- get_default_handlers()\n",
      "server_tools$simplify_view()       # Toggle optional view",
      "server_tools$simplify_view('yes')  # Show optional view",
      "server_tools$simplify_view('no')   # Hide optional view"
    )
  )

  reactive_handlers$run_analysis_flag <- structure(
    run_analysis_flag, class = "ravedash_printable",
    docs = paste(
      sep = "\n",
      "Flag to run analysis pipeline. Usage:\n",
      "# Obtain the server utility functions",
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
      "server_tools <- get_default_handlers()\n",
      'server_tools$auto_recalculate(TRUE)    # Turn on forever',
      'server_tools$auto_recalculate(FALSE)   # Turn off forever',
      'server_tools$auto_recalculate(1)       # Turn on once'
    )
  )

  reactive_handlers$module_is_active <- structure(
    module_is_active, class = "ravedash_printable",
    docs = paste(
      sep = "\n",
      "Shiny reactive to tell if the module is active or hidden. Usage:\n",
      "# Obtain the server utility functions",
      "server_tools <- get_default_handlers()\n",
      'server_tools$module_is_active()                   # current module',
      'server_tools$module_is_active("another_module")   # whether another module is active'
    )
  )

  # Add-on, register preset components
  if(is.environment(parse_env) || is.list(parse_env)){
    local({

      nms <- names(parse_env)
      for(nm in nms){
        item <- parse_env[[nm]]
        if(inherits(item, "RAVEShinyComponentContainer")){
          logger("Found a RAVEShinyComponentContainer: ", nm, ". validating", level = "trace")
          item$validate_server(session)
          lapply(names(item$components), function(nm){
            logger("Registering component: ", nm, "...", level = "trace")
            item$components[[nm]]$server_func(input, output, session)
          })
        }
      }

    })


  }


#   tryCatch({
#     force(settings_file)
#     pipeline_path <- raveio::pipeline_find(pipeline_name)
#     pipeline_settings_path <- file.path(pipeline_path, settings_file)
#
#     pipeline_settings <- local({
#       settings <- raveio::load_yaml(pipeline_settings_path)
#       list(
#         set = function(...){
#           args <- list(...)
#           argnames <- names(args)
#           if(!length(argnames)){
#             return(as.list(settings))
#           }
#           args <- args[argnames != ""]
#           argnames <- names(args)
#           if(!length(argnames)){
#             return(as.list(settings))
#           }
#           for(nm in argnames){
#             settings[[nm]] <<- args[[nm]]
#           }
#           raveio::save_yaml(x = settings, file = pipeline_settings_path)
#           return(as.list(settings))
#         },
#         get = function(key, default = NA){
#           if(missing(key)){
#             return(as.list(settings))
#           }
#           if(!settings$`@has`(key)){
#             return(default)
#           }
#           settings[[key]]
#         }
#       )
#     })
#     pipeline_set <- pipeline_settings$set
#     pipeline_get <- function(key, missing = NULL, constraint){
#       re <- pipeline_settings$get(key, missing)
#       if(!missing(constraint)){
#         re <- re %OF% constraint
#       }
#       re
#     }
#
#     component_container <- ravedash:::RAVEShinyComponentContainer$new(
#       module_id = module_id, pipeline_name = pipeline_name,
#       settings_file = settings_file, pipeline_path = pipeline_path
#     )
#
#     component_container$validate_server(session)
#
#     reactive_handlers$has_pipeline <- TRUE
#     reactive_handlers$pipeline_name <- pipeline_name
#     reactive_handlers$pipeline_path <- pipeline_path
#     reactive_handlers$pipeline_get <- structure(
#       pipeline_get, class = "ravedash_printable",
#       docs = paste(
#         sep = "\n",
#         "Get variables from last executed pipeline. Usage:\n",
#         "server_tools <- get_default_handlers()\n",
#         "# get variable called `varname`",
#         'server_tools$pipeline_get("varname", missing = NULL)\n',
#         "# get variable with constraint",
#         'server_tools$module_is_active("varname", constraint = c("A", "B", "C"))'
#       )
#     )
#     reactive_handlers$pipeline_set <- pipeline_set
#     reactive_handlers$component_container <- component_container
#
#
#   }, error = function(e) {
#     reactive_handlers$has_pipeline <- FALSE
#     logger("Unable to register component container: \n", e$message, level = "warning")
#   })

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