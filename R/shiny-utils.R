# icons
#' @title Shiny icons
#' @export
shiny_icons <- list(
  bars = shiny::icon("bars"),
  grid = shiny::icon("th"),
  keyboard = shiny::icon("keyboard"),
  help = shiny::icon('question-circle'),
  sync = shiny::icon('sync'),
  expand = shiny::icon('expand'),
  tasks = shiny::icon('tasks'),
  angle_right = shiny::icon('angle-right'),
  arrow_right = shiny::icon('arrow-right'),
  external_link = shiny::icon('external-link-alt'),
  plus = shiny::icon('plus'),
  minus = shiny::icon('minus'),
  download = shiny::icon("download"),
  save = shiny::icon("save"),
  trash = shiny::icon("trash"),
  export = shiny::icon("file-export"),
  puzzle = shiny::icon("puzzle-piece"),
  user_md = shiny::icon("user-md"),
  image = shiny::icon("file-image"),
  magic = shiny::icon("magic")
)

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
get_rave_event <- function(key, session = shiny::getDefaultReactiveDomain()){
  force(key)
  tool <- register_rave_session(session)

  return(tool$rave_event[[key]])
}

#' @export
open_loader <- function(session = shiny::getDefaultReactiveDomain()){
  fire_rave_event('open_loader', Sys.time())
}

#' @export
close_loader <- function(session = shiny::getDefaultReactiveDomain()){
  fire_rave_event('open_loader', NULL)
}

#' @export
watch_loader_opened <- function(session = shiny::getDefaultReactiveDomain()){
  tool <- register_rave_session(session)
  res <- tool$rave_event$open_loader
  structure(!is.null(res), timestamp = res)
}

#' @export
watch_data_loaded <- function(session = shiny::getDefaultReactiveDomain()){
  tool <- register_rave_session(session)
  res <- tool$rave_event$data_loaded
  structure(length(res) && !isFALSE(res), timestamp = res)
}

#' @export
current_shiny_theme <- function(default, session = shiny::getDefaultReactiveDomain()){
  if(dipsaus::shiny_is_running()) {
    tool <- register_rave_session(session = session)
    return(shidashi::get_theme(tool$theme_event, session = session))
  } else {
    if(missing(default)){
      default <- list(theme = "light", background = "#FFFFFF", foreground = "#000000")
    } else {
      default <- as.list(default)[c("theme", "background", "foreground")]
      default$theme %?<-% "light"
      default$background %?<-% "#FFFFFF"
      default$foreground %?<-% "#000000"
    }
    return(default)
  }
}

#' @export
footer_crumb <- function(module_id = NULL){
  ns <- shiny::NS(module_id)
  shiny::div(
    class = "back-to-top",
    shiny::div(
      class = "btn-group dropup",
      role="group",
      # shiny::a(
      #   type="button", class="btn btn-default btn-go-top border-right-1", href="#",
      #   shidashi::as_icon("rocket")
      # ),
      shiny::tags$button(
        type="button",
        id = ns("loader_short_message"),
        class="btn btn-default border-right-1 btn-go-top shiny-text-output rave-button",
        `data-toggle` = "tooltip",
        title = "Click to toggle the data loader",
        `rave-action` = '{"type": "loader_toggle"}'
      ),
      shiny::tags$button(
        type="button",
        class="btn btn-default btn-go-top border-left-1 dropdown-toggle dropdown-toggle-split", href="#",
        "data-toggle"="dropdown",
        "aria-haspopup"="false",
        "aria-expanded"="false",
        shiny::span(
          class = "sr-only",
          "Dropdown-Open"
        )
      ),
      shiny::div(
        class = "dropdown-menu dropdown-menu-right",
        title = "Jump to:"
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
add_html_class <- function(selector, class,
                           session = shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage("shidashi.add_class", list(
    selector = selector,
    class = class
  ))
}

#' @export
remove_html_class <- function(selector, class,
                              session = shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage("shidashi.remove_class", list(
    selector = selector,
    class = class
  ))
}

#' @export
run_analysis_button <- function(label = "Run analysis (Ctrl+Enter)", icon = NULL, width = NULL, type = "primary",
                                btn_type = "button", class = "", ...){
  if (length(type) > 1) {
    type <- type[[1]]
  }
  stopifnot2(length(type) == 0 || type[[1]] %in% c("default",
                                                   "primary", "info", "success", "warning", "danger"), msg = "type must be in 'default', 'primary', 'info', 'success', 'warning', 'danger'")

  args <- list(...)
  style <- c(args[["style"]], "")[[1]]
  width <- c(width, "auto")[[1]]
  style <- paste0("width: ", shiny::validateCssUnit(width),
                  ";", style)
  class <- dipsaus::combine_html_class(
    sprintf("btn btn-%s rave-button %s", type, class))

  shiny::tags$button(
    class = class,
    style = style,
    type = btn_type,
    "rave-action" = '{"type": "run_analysis"}',
    list(shidashi::as_icon(icon), label)
  )
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

