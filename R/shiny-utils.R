#' @export
register_rave_session <- function(session = shiny::getDefaultReactiveDomain(), .rave_id){
  if(is.null(session)){
    session <- shiny::MockShinySession$new()
  }
  sync_tools <- shidashi::register_session_id(session = session)
  event_data <- shidashi::register_session_events(session = session)
  rave_event <- shidashi::register_global_reactiveValues(name = "rave_reactives", session = session)

  root_session <- session$rootScope()
  if(!root_session$cache$exists('rave_id')){
    if(!missing(.rave_id)){
      rave_id <- paste(unlist(.rave_id), collapse = "")
      rave_id <- gsub("[^a-zA-Z0-9]", "", rave_id)
      if(nchar(rave_id) == 0){
        rave_id <- rand_string()
      }
    } else {
      rave_id <- rand_string()
    }
    root_session$cache$set('rave_id', rave_id)
    root_session$userData$rave_id <- rave_id
  }
  rave_id <- root_session$cache$get("rave_id")
  if(!root_session$cache$exists('rave_loop_events')){
    rave_loop_events <- dipsaus::fastqueue2()
    root_session$cache$set('rave_loop_events', rave_loop_events)
  }
  rave_loop_events <- root_session$cache$get("rave_loop_events")

  if(!inherits(session, "MockShinySession")){
    # register session
    item <- list(
      rave_id = rave_id,
      register_ns = session$ns(NULL),
      root_session = root_session,
      rave_event = rave_event
    )

    sess <- get(x = '.sessions')
    sess$set(rave_id, item)
  }

  list(
    rave_id = rave_id,
    sync_tools = sync_tools,
    theme_event = event_data,
    rave_event = rave_event,
    loop_event = rave_loop_events
  )
}

#' @export
fire_rave_event <- function(key, value, global = FALSE, force = FALSE, session = shiny::getDefaultReactiveDomain()){
  force(key)
  force(value)
  tool <- register_rave_session(session)

  if(global) {
    sess <- get(x = '.sessions')
    lapply(sess$keys(), function(key){
      item <- sess$get(key, missing = NULL)
      if(is.null(item)){ return() }
      if(!is.list(item)){
        sess$remove(key)
        return()
      }
      if(!is.environment(item$root_session)){
        sess$remove(key)
        return()
      }
      root_session <- item$root_session
      if( root_session$isEnded() ){
        sess$remove(key)
        return()
      }
      # shiny::isolate({
      #   # impl <- .subset2(item$rave_event, "impl")
      #   # impl$set(key, value, isTRUE(force))
      #
      # })
      item$rave_event[[key]] <- value

    })
  } else if(!session$isEnded()){
    # impl <- .subset2(tool$rave_event, "impl")
    # impl$set(key, value, isTRUE(force))
    tool$rave_event[[key]] <- value
  }
  invisible()
}

#' @export
footer_crumb <- function(module_id = NULL){
  ns <- shiny::NS(module_id)
  shiny::div(
    class = "back-to-top",
    shiny::div(
      class = "btn-group dropup",
      role="group",
      shiny::a(
        type="button", class="btn btn-default btn-go-top border-right-1", href="#",
        shidashi::as_icon("rocket")
      ),
      shiny::tags$button(
        type="button",
        class="btn btn-default border-left-1 btn-go-top action-button",
        id = ns("loader_show"),
        shiny::textOutput(ns("loader_short_message"), inline = TRUE),
        `data-toggle` = "tooltip",
        title = "Click to toggle the data loader"
      )
    )
  )

}



get_active_module_info <- function(session = shiny::getDefaultReactiveDomain()){
  if(is.environment(session)){
    rave_events <- session$cache$get("rave_reactives", missing = NULL)
    if(shiny::is.reactivevalues(rave_events)){
      info <- shiny::isolate({
        rave_events$active_module
      })
      return(info)
    }
  }
  return(NULL)
}

#' @export
logger_threshold <- function(
  level = c("info", "warning", "error", "fatal", "debug", "trace"),
  module_id, ...){

  level <- match.arg(level)

  if(missing(module_id)){
    module <- get_active_module_info()
    if(is.list(module)){
      namespace <- module$id
    } else {
      namespace <- "ravedash"
    }
  } else {
    namespace <- module_id
  }

  loglevel <- switch (
    level,
    'info' = logger::INFO,
    "warning" = logger::WARN,
    "error" = logger::ERROR,
    "fatal" = logger::FATAL,
    "debug" = logger::DEBUG,
    {
      logger::TRACE
    }
  )
  logger::log_threshold(level = loglevel, namespace = namespace, ...)
}

#' @export
logger <- local({
  last_time <- NULL
  function(..., level = c("info", "warning", "error", "fatal", "debug", "trace"),
           calc_delta = 'auto', .envir = parent.frame()){
    level <- match.arg(level)
    module <- get_active_module_info()
    if(is.list(module)){
      namespace <- module$id
    } else {
      namespace <- "ravedash"
    }

    if(identical(calc_delta, "auto") && level %in% c("debug", "trace")){
      calc_delta <- TRUE
    }
    calc_delta <- isTRUE(calc_delta)


    now <- Sys.time()
    if(calc_delta){
      if(is.null(last_time)){
        delta <- 0
      } else {
        delta <- dipsaus::time_delta(last_time, now)
      }
      last_time <<- now
      if(delta < 0.3){
        delta_color <- "crayon::silver"
      } else if(delta < 0.5){
        delta_color <- "crayon::cyan"
      } else if (delta < 1){
        delta_color <- "crayon::green"
      } else if (delta < 5){
        delta_color <- "crayon::yellow"
      } else {
        delta_color <- "crayon::red"
      }
      delta <- sprintf("(+%.2fs)", delta)
    } else {
      delta_color <- "crayon::silver"
      delta <- ""
      last_time <<- NULL
    }

    logger::log_formatter(logger::formatter_glue, namespace = namespace)
    # logger::log_formatter(logger::formatter_glue, namespace = "ravedash")

    loglevel <- switch (
      level,
      'info' = logger::INFO,
      "warning" = logger::WARN,
      "error" = logger::ERROR,
      "fatal" = logger::FATAL,
      "debug" = logger::DEBUG,
      {
        logger::TRACE
      }
    )

    level_str <- attr(loglevel, "level")

    if(dipsaus::package_installed("crayon") && crayon::has_color()){

      if(calc_delta){
        layout <- logger::layout_glue_generator(format = sprintf("{crayon::bold(logger::colorize_by_log_level(msg = level, level = levelr))} {crayon::silver(paste0(format(time, \"%%Y-%%m-%%d %%H:%%M:%%S\"), ifelse(ns == 'ravedash', '', paste0(' ', ns))))} {%s('%s')} {logger::colorize_by_log_level(msg = msg, level = levelr)}", delta_color, delta))
      } else {
        layout <- logger::layout_glue_generator(format = "{crayon::bold(logger::colorize_by_log_level(msg = level, level = levelr))} {crayon::silver(paste0(format(time, \"%Y-%m-%d %H:%M:%S\"), ifelse(ns == 'ravedash', '', paste0(' ', ns))))} {logger::colorize_by_log_level(msg = msg, level = levelr)}")
      }


    } else {
      if(calc_delta){
        layout <- logger::layout_glue_generator(format = sprintf("{level} [{paste0(format(time, \"%%Y-%%m-%%d %%H:%%M:%%S\"), ifelse(ns == 'ravedash', '', paste0(' ', ns)), ' %s')}] {msg}", delta))
      } else {
        layout <- logger::layout_glue_generator(format = "{level} [{paste0(format(time, \"%Y-%m-%d %H:%M:%S\"), ifelse(ns == 'ravedash', '', paste0(' ', ns)))}] {msg}")
      }

    }
    logger::log_layout(layout = layout, namespace = namespace)

    logger::log_level(level = loglevel, namespace = namespace, raveio::glue(..., .envir = .envir))

  }
})

