


safe_wrap_expr <- function(expr, onFailure = NULL, finally = {}, log_error = "error"){

  # rlang::try_fetch({
  #   force(expr)
  # }, error = function(e){
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
  #   if(is.function(onFailure)){
  #     onFailure(e)
  #   }
  #
  #   logger_error_condition(e)
  #   # logger(paste(c("Wrapped expressions:", deparse(expr_)), collapse = "\n"),
  #   #        .sep = "\n", level = log_error, use_glue = FALSE)
  #
  #   if(dipsaus::shiny_is_running()) {
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
  }, error = function(e){
    if(is.function(onFailure)){
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
                    watch_data = getOption("ravedash.auto_watch_data", FALSE)){
  error_wrapper <- match.arg(error_wrapper)
  if(!quoted){
    x <- substitute(x)
  }
  if(watch_data) {
    x <- bquote({
      if(!shiny::isolate(asNamespace('ravedash')$watch_data_loaded())) {
        asNamespace('ravedash')$logger("Data not loaded...")
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
        asNamespace('ravedash')$safe_wrap_expr(.(x))
      })
    },
    "notification" = {
      x <- bquote({
        asNamespace('ravedash')$safe_wrap_expr({
          asNamespace("ravedash")$with_error_notification(.(x))
        })
      })
    },
    "alert" = {
      x <- bquote({
        asNamespace('ravedash')$safe_wrap_expr({
          asNamespace("ravedash")$with_error_alert(.(x))
        })
      })
    }
  )


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

#' @title Register shiny-output options to allow display in stand-alone viewers
#' @description Save the output options such that the additional configurations
#' can be used by stand-alone viewer
#' @param outputId the full shiny output ID
#' @param session shiny session object
#' @param module_session the module shiny session; if not provided, then
#' the session will be inferred by \code{rave_id}
#' @param rave_id the unique identification key for 'RAVE' module sessions,
#' can be obtained via \code{\link{get_active_module_info}}
#' @param wrapper_id the wrapping render ID, default is \code{"viewer"}
#' @return nothing
#' @details 'RAVE' dashboard provides powerful stand-alone viewers where
#' users can display almost any outputs from other modules and interact with
#' these viewers while sending messages back.
#'
#' @examples
#' if(interactive()) {
#'
#' library(shiny)
#' library(ravedash)
#'
#' rave_id <- paste(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE),
#'                  collapse = "")
#'
#' ui <- function(req) {
#'   query_string <- req$QUERY_STRING
#'   if(length(query_string) != 1) {
#'     query_string <- "/"
#'   }
#'   query_result <- httr::parse_url(query_string)
#'
#'   if(!identical(toupper(query_result$query$standalone), "TRUE")) {
#'     # normal page
#'     basicPage(
#'       actionButton("btn", "Click Me"),
#'       plotOutput("plot")
#'     )
#'   } else {
#'     # standalone viewer
#'     uiOutput("viewer")
#'   }
#' }
#'
#' server <- function(input, output, session) {
#'
#'   bindEvent(
#'     safe_observe({
#'       query_string <- session$clientData$url_search
#'       query_result <- httr::parse_url(query_string)
#'
#'       if(!identical(toupper(query_result$query$standalone), "TRUE")) {
#'         # normal page
#'         register_rave_session(session = session, .rave_id = rave_id)
#'         output$plot <- renderPlot({
#'           input$btn
#'           plot(rnorm(100), pch = 16)
#'         })
#'       } else {
#'         # standalone viewer
#'         standalone_viewer(outputId = "plot", rave_id = rave_id)
#'       }
#'     }),
#'     session$clientData$url_search
#'   )
#'
#'
#' }
#'
#' shinyApp(ui, server, options = list(port = 8989))
#'
#' # Now open http://127.0.0.1:8989/?standalone=TRUE
#'
#' }
#'
#' @export
standalone_viewer <- function(
    outputId, module_session, rave_id,
    session = shiny::getDefaultReactiveDomain(),
    wrapper_id = "viewer"
) {

  root_session <- session$rootScope()
  local_data <- dipsaus::fastmap2()

  # query_string <- "/?type=widget&output_id=plot_overall&rave_id=Pnd8MuxNVsZGcbrRWn8G&module=standalone_viewer"

  if(length(outputId) != 1 || is.na(outputId) || !nchar(outputId)) {
    stop("The output ID is blank or invalid.")
  }

  # get module session
  if(missing(module_session) || !is.environment(module_session)) {
    module_session <- get_session_by_rave_id(rave_id)
    if(is.null(module_session)) {
      stop("There is no shiny-session with provided `rave_id`. Please specify a valid `module_session` or `rave_id`")
    }
  }
  module_id <- module_session$ns(NULL)
  # make sure we are working on the root scope
  module_session <- module_session$rootScope()

  # get module information
  module_info <- get_active_module_info(session = module_session)
  if(!is.null(module_info)) {
    module_id <- module_info$id
  }

  logger("Preparing standalone viewer for output `{outputId}` (module: {module_id})", level = "trace", use_glue = TRUE)

  # get renderer's information
  reactive_handlers <- get_default_handlers(session = module_session)
  output_options <- reactive_handlers$output_options
  ns2 <- shiny::NS(module_id)
  render_function <- module_session$getOutput(ns2(outputId))

  if(!is.function(render_function)) {
    stop("Cannot find render function for output: ", outputId)
  }

  # get output function
  ui_function <- attr(render_function, "outputFunc")
  ns2 <- shiny::NS(module_id)
  full_outputId <- ns2(outputId)

  # Alternating default session to module's session
  shiny::withReactiveDomain(module_session, {

    root_session$output[[wrapper_id]] <- shiny::renderUI({

      opts <- as.list(output_options[[full_outputId]])
      # get output options
      output_args <- c(
        list(full_outputId),
        as.list(opts$args)
      )

      output_args2 <- output_args
      output_args2$width <- "100vw"
      output_args2$height <- "100vh"

      tryCatch({
        do.call(ui_function, output_args2)
      }, error = function(e) {
        do.call(ui_function, output_args)
      })
    })

    root_session$output[[full_outputId]] <- render_function

    observe({
      inputs <- shiny::reactiveValuesToList(root_session$input)
      nms <- names(inputs)
      nms <- nms[startsWith(nms, ns2("")) & !startsWith(nms, "@")]
      if(length(nms)) {
        for(nm in nms) {
          signature <- dipsaus::digest(inputs[[nm]])
          if(!identical(signature, local_data[[nm]])) {
            dipsaus::set_shiny_input(
              session = module_session,
              inputId = nm,
              value = inputs[[nm]],
              priority = "event",
              method = "proxy"
            )
            local_data[[nm]] <- signature
          }
        }
      }
    })

  })

}
