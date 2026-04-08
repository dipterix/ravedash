#' @name rave-runtime-events
#' @title 'RAVE' run-time events
#' @description A set of preset behaviors used by 'RAVE' modules
#' @param session shiny session, usually automatically determined
#' @param rave_id,.rave_id deprecated, no longer used
#' @param key event key to fire or to monitor
#' @param value event value
#' @param global whether to notify other sessions (experimental and not
#' recommended)
#' @param force whether to force firing the event even the \code{value} hasn't
#' changed
#' @param default default value if not found
#' @param .internal_ok internally used
#' @details These goal of these event functions is to  simplify the dashboard
#' logic without understanding the details or passing global variables around.
#' Everything starts with \code{shidashi::register_session}. This function
#' registers session event handlers and theme observers.
#' If you have called \code{\link{module_server_common}}, then session
#' registration has already been done.
#' \describe{
#' \item{\code{register_rave_session}}{\strong{Deprecated.} Use
#' \code{shidashi::register_session()} instead.}
#' \item{\code{fire_rave_event}}{send signals to make changes to a event;
#' returns nothing}
#' \item{\code{get_rave_event}}{watch and get the event values; must run in
#' shiny reactive context}
#' \item{\code{open_loader}}{fire an event with a special key
#' \code{'open_loader'} to open the data-loading panel; returns nothing}
#' \item{\code{close_loader}}{reset an event with a special key
#' \code{'open_loader'} to close the data-loading panel if possible;
#' returns nothing}
#' \item{\code{watch_loader_opened}}{watch in shiny reactive context whether
#' the loader is opened; returns a logical value, but raise errors when
#' reactive context is missing}
#' \item{\code{watch_data_loaded}}{watch a special event with key
#' \code{'data_loaded'}; returns a logical value of whether new data has been
#' loaded, or raise errors when reactive context is missing}
#' \item{\code{current_shiny_theme}}{watch and returns a list of theme
#' parameters, for example, light or dark theme}
#' }
#' @section Built-in Events:
#' The following event keys are built-in. Please do not fire them using
#' \code{fire_rave_event} or the 'RAVE' application might will crash
#' \describe{
#' \item{\code{'simplify_toggle'}}{toggle visibility of 'HTML' elements with
#' class \code{'rave-option'}}
#' \item{\code{'run_analysis'}}{notifies the module to run pipeline}
#' \item{\code{'save_pipeline'}, \code{'load_pipeline'}}{notifies the module to
#' save or load pipeline}
#' \item{\code{'data_loaded'}}{notifies the module that new data has been
#' loaded}
#' \item{\code{'open_loader'}, \code{'toggle_loader'}}{notifies the internal
#' server code to show or hide the data loading panel}
#' \item{\code{'active_module'}}{internally populated by the action
#' dispatcher when the module first loads (and whenever a
#' \code{type = "active_module"} rave-action arrives). Returns a list with
#' elements \code{type}, \code{id}, \code{parent_frame}, and \code{rave_id}.
#' This key is \strong{reserved}: external code must not call
#' \code{fire_rave_event("active_module", ...)} directly.
#' To obtain the currently active module, prefer
#' \code{\link{get_active_module_info}()}, which reads from the
#' \code{shidashi} module registry and is always up-to-date.}
#' }
#' @return See 'Details'
#'
#' @examples
#'
#'
#' library(shiny)
#' library(ravedash)
#'
#' ui <- fluidPage(
#'   actionButton("btn", "Fire event"),
#'   actionButton("btn2", "Toggle loader")
#' )
#'
#' server <- function(input, output, session) {
#'   # Create event registries
#'   shidashi::register_session()
#'
#'   shiny::bindEvent(
#'     shiny::observe({
#'       fire_rave_event("my_event_key", Sys.time())
#'     }),
#'     input$btn,
#'     ignoreInit = TRUE,
#'     ignoreNULL = TRUE
#'   )
#'   shiny::bindEvent(
#'     shiny::observe({
#'       cat("An event fired with value:", get_rave_event("my_event_key"), "\n")
#'     }),
#'     get_rave_event("my_event_key"),
#'     ignoreNULL = TRUE
#'   )
#'
#'   shiny::bindEvent(
#'     shiny::observe({
#'       if (watch_loader_opened()) {
#'         close_loader()
#'       } else {
#'         open_loader()
#'       }
#'     }),
#'     input$btn2,
#'     ignoreInit = TRUE,
#'     ignoreNULL = TRUE
#'   )
#'
#'   shiny::bindEvent(
#'     shiny::observe({
#'       cat("Loader ", ifelse(watch_loader_opened(), "opened", "closed"), "\n")
#'     }),
#'     watch_loader_opened(),
#'     ignoreNULL = TRUE
#'   )
#'
#' }
#'
#' if(interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @export
register_rave_session <- function(
  session = shiny::getDefaultReactiveDomain(),
  .rave_id = NULL
) {
  .Deprecated(
    msg = paste(
      "`register_rave_session()` is deprecated.",
      "Use `shidashi::register_session()` instead."
    )
  )
  if (is.null(session)) {
    session <- shiny::MockShinySession$new()
  }
  root_session <- session$rootScope()

  # make sure ravedash is in `userData`
  if (!inherits(root_session$userData$ravedash, "fastmap2")) {
    root_session$userData$ravedash <- dipsaus::fastmap2()
  }
  map <- root_session$userData$ravedash

  # register with shidashi session registry (installs event bus and theme observer)
  shidashi::register_session(session = session)

  # reactive event handlers
  if (!inherits(map$handlers, "fastmap2")) {
    handler_map <- dipsaus::fastmap2()
    handler_map$output_options <- dipsaus::fastmap2()
    map$handlers <- handler_map
  }

  invisible()
}

# clean_shiny_sessions moved to R/deprecated.R

#' @rdname rave-runtime-events
#' @export
get_default_handlers <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    session <- shiny::MockShinySession$new()
  }
  root_session <- session$rootScope()
  if (!inherits(root_session$userData$ravedash, "fastmap2")) {
    root_session$userData$ravedash <- dipsaus::fastmap2()
  }
  if (!inherits(root_session$userData$ravedash$handlers, "fastmap2")) {
    handler_map <- dipsaus::fastmap2()
    handler_map$output_options <- dipsaus::fastmap2()
    root_session$userData$ravedash$handlers <- handler_map
  }
  root_session$userData$ravedash$handlers
}

#' @rdname rave-runtime-events
#' @export
fire_rave_event <- function(
  key,
  value,
  global = FALSE,
  force = FALSE,
  session = shiny::getDefaultReactiveDomain(),
  .internal_ok = FALSE
) {
  force(key)

  if (!.internal_ok && key %in% c("active_module")) {
    logger(
      "`fire_rave_event`: key 'active_module' is reserved. Do not manually set this key.",
      level = "error"
    )
    return(invisible())
  }

  force(value)

  logger("Firing RAVE-event: ", key, level = "trace")

  if (!session$isEnded()) {
    shidashi::fire_event(key, value, session = session, global = global)
  }
  invisible()
}

# get_session_by_rave_id moved to R/deprecated.R

#' @rdname rave-runtime-events
#' @export
get_rave_event <- function(key, session = shiny::getDefaultReactiveDomain()) {
  force(key)
  return(shidashi::get_event(key, session = session))
}

#' @rdname rave-runtime-events
#' @export
open_loader <- function(session = shiny::getDefaultReactiveDomain()) {
  fire_rave_event('open_loader', Sys.time())
}

#' @rdname rave-runtime-events
#' @export
close_loader <- function(session = shiny::getDefaultReactiveDomain()) {
  fire_rave_event('open_loader', NULL)
}

#' @rdname rave-runtime-events
#' @export
watch_loader_opened <- function(session = shiny::getDefaultReactiveDomain()) {
  res <- shidashi::get_event("open_loader", session = session)
  structure(!is.null(res), timestamp = res)
}

#' @rdname rave-runtime-events
#' @export
watch_data_loaded <- function(session = shiny::getDefaultReactiveDomain()) {
  res <- shidashi::get_event("data_loaded", session = session)

  # 1. load from pipeline settings only
  # 2. combinations of subject default and pipeline settings

  if (length(res) && is.list(res)) {
    return(structure(
      TRUE,
      timestamp = res$timestamp,
      force = isTRUE(res$force)
    ))
  }
  structure(length(res) && !isFALSE(res),
            timestamp = res,
            force = FALSE)
}

#' @rdname rave-runtime-events
#' @export
current_shiny_theme <- function(
  default,
  session = shiny::getDefaultReactiveDomain()
) {
  if (shiny_is_running()) {
    return(shidashi::get_theme(session = session))
  } else {
    if (missing(default)) {
      default <- list(
        theme = "light",
        background = "#FFFFFF",
        foreground = "#000000"
      )
    } else {
      default <- as.list(default)[c("theme", "background", "foreground")]
      default$theme %?<-% "light"
      default$background %?<-% "#FFFFFF"
      default$foreground %?<-% "#000000"
    }
    return(default)
  }
}

#' A hovering footer at bottom-right
#' @description Internally used. Do not call explicitly
#' @param module_id 'RAVE' module ID
#' @param label run-analysis button label; default is \code{"Run Analysis"}
#' @param auto_recalculation whether to show the automatic calculation button;
#' default is true
#' @param message_action message to send when clicking on message button;
#' default is \code{'toggle_loader'}, which opens up loading screen
#' @param class additional class for the footer
#' @param style additional style for the footer
#' @return 'HTML' tags
#'
#' @examples
#'
#' library(shiny)
#' # dummy variables for the example
#' data_loaded <- TRUE
#'
#' # UI code
#' ravedash_footer("my_module")
#'
#' # server code to set message
#' server <- function(input, output, session) {
#'
#'   module_server_common(input, output, session, function() {
#'
#'     # check if data has been loaded
#'     if (data_loaded) {
#'
#'       # if yes, then set the footer message
#'       fire_rave_event("loader_message",
#'                       "my_project/subject - Epoch: Auditory")
#'       return(TRUE)
#'     } else {
#'
#'       # No data found, unset the footer message
#'       fire_rave_event("loader_message", NULL)
#'       return(FALSE)
#'     }
#'
#'   })
#' }
#'
#' @export
ravedash_footer <- function(
    module_id = NULL, label = "Run Analysis",
    auto_recalculation = TRUE, message_action = "toggle_loader",
    class = NULL, style = NULL
) {
  ns <- shiny::NS(module_id)
  shiny::div(
    class = shidashi::combine_html_class(
      "ravedash-back-to-top ravedash-footer",
      class
    ),
    style = style,
    shiny::div(
      class = "btn-group dropup",
      role = "group",
      # shiny::a(
      #   type = "button",
      #   class = "btn btn-default btn-go-top border-right-1",
      #   href = "#",
      #   shidashi::as_icon("rocket")
      # ),
      shiny::tags$button(
        type = "button",
        id = ns("__loader_short_message__"),
        class = "btn btn-default border-right-1 btn-go-top shiny-text-output rave-button",
        `data-toggle` = "tooltip",
        title = "Click to toggle the data loader",
        `rave-action` = sprintf('{"type": "%s"}', message_action)
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-default border-right-1 border-left-1 rave-button rave-button-autorecalculate",
        `data-toggle` = "tooltip",
        title = "Keyboard shortcut: CTRL+Enter / Command+Return (OSX)",
        `rave-action` = '{"type": "run_analysis"}',
        label
      ),
      shiny::tags$button(
        type = "button",
        class = "btn btn-default btn-go-top border-left-1 dropdown-toggle dropdown-toggle-split",
        href = "#",
        # "data-toggle"="dropdown",
        "aria-haspopup" = "false",
        "aria-expanded" = "false",
        shiny::span(
          class = "sr-only",
          "Dropdown-Open"
        )
      ),
      shiny::div(
        class = "dropdown-menu dropdown-menu-right",
        shiny::h6(
          class = "dropdown-header",
          "Controllers"
        ),
        # shiny::a(
        #   class = "dropdown-item rave-button",
        #   href = "#",
        #   `rave-action` = '{"type": "toggle_auto_recalculation"}',
        #   shiny_icons$sync,
        #   "Auto re-calculation: ", shiny::textOutput(
        #     outputId = ns("__recalculation_message__"),
        #     container = function(...) {
        #       shiny::span(style = "color: #007bff", ...)
        #     })
        # ),
        shiny::div(
          class = "px-3 py-1",
          local({
            if (auto_recalculation) {
              shiny::a(
                class = "btn btn-default rave-button",
                href = "#",
                `rave-action` = '{"type": "toggle_auto_recalculation"}',
                `data-toggle` = "tooltip",
                title = "Toggle auto re-calculation",
                shiny_icons["sync"],
                shiny::textOutput(
                  outputId = ns("__recalculation_message__"),
                  container = function(..., class = NULL) {
                    shiny::span(
                      style = "color: #007bff",
                      ...,
                      class = shidashi::combine_html_class(
                        class,
                        "pointer-events-none"
                      )
                    )
                  }
                )
              )
            } else {
              NULL
            }
          }),
          shiny::a(
            class = "btn btn-default shidashi-button",
            href = "#",
            `shidashi-action` = '{"method": "card", "args": [{"selector": ".ravedash-input-card", "method": "expand"}]}',
            `data-toggle` = "tooltip",
            title = "Expand all input cards",
            shiny_icons['plus']
          ),
          shiny::a(
            class = "btn btn-default shidashi-button",
            href = "#",
            `shidashi-action` = '{"method": "card", "args": [{"selector": ".ravedash-input-card", "method": "collapse"}]}',
            `data-toggle` = "tooltip",
            title = "Collapse all input cards",
            shiny_icons['minus']
          ),
          shiny::a(
            class = "btn btn-default rave-button",
            href = "#",
            `rave-action` = '{"type": "simplify_toggle"}',
            shiny_icons['simplybuilt'],
            `data-toggle` = "tooltip",
            title = "Show more/fewer options"
          )
        ),
        local({
          if (
            ravepipeline::raveio_getopt(
              "interactive_debugging",
              default = FALSE
            )
          ) {
            shiny::div(
              class = "px-3 py-1",
              shiny::a(
                class = "btn btn-default rave-button",
                href = "#",
                `rave-action` = '{"type": "interactive_debugging"}',
                shiny_icons["terminal"],
                `data-toggle` = "tooltip",
                title = "Interactive debugging"
              )
            )
          } else {
            NULL
          }
        }),
        # shiny::a(
        #   class = "dropdown-item shidashi-button",
        #   href = "#",
        #   `shidashi-action` = '{"method": "card", "args": [{"selector": ".ravedash-input-card", "method": "expand"}]}',
        #   shiny_icons$plus,
        #   "Expand all input cards"
        # ),
        # shiny::a(
        #   class = "dropdown-item shidashi-button",
        #   href = "#",
        #   `shidashi-action` = '{"method": "card", "args": [{"selector": ".ravedash-input-card", "method": "collapse"}]}',
        #   shiny_icons$minus,
        #   "Collapse all input cards"
        # ),
        # shiny::a(
        #   class = "dropdown-item rave-button",
        #   href = "#",
        #   `rave-action` = '{"type": "simplify_toggle"}',
        #   shiny_icons$simplybuilt,
        #   "Show more/fewer options"
        # ),
        shiny::div(
          class = "dropdown-divider"
        ),
        shiny::h6(
          class = "dropdown-header",
          "Quick Access"
        )
      )
    )
  )

}

#' Get current active module information, internally used
#' @param session shiny reactive domain, default is current domain
#' @return A named list, including module ID, module label, internal
#' \code{'rave_id'}.
#' @export
get_active_module_info <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.environment(session)) {
    info <- shiny::isolate({
      shidashi::active_module(session = session)
    })
    # make sure module_id is inside!!!
    if (!"id" %in% names(info)) {
      return(NULL)
    }

    info$rave_id <- session$token

    # get session information
    session_path <- current_session_path()
    if (is.null(session_path)) {
      session_path <- "."
    }
    info$path <- normalizePath(
      file.path(session_path, "modules", info$id),
      mustWork = FALSE
    )

    return(info)
  }
  return(NULL)
}

#' @rdname get_active_module_info
#' @export
get_active_pipeline <- function(
    session = shiny::getDefaultReactiveDomain()) {

  module_info <- get_active_module_info(session = session)
  if (!is.list(module_info) || is.null(module_info$id)) {
    return(NULL)
  }

  ravepipeline::pipeline(pipeline_name = module_info$id,
                   paths = dirname(module_info$path))

}

#' Button to trigger analysis
#' @description A button that triggers \code{'run_analysis'} event;
#' see also \code{\link{get_rave_event}}
#' @param label label to display
#' @param icon icon before the label
#' @param type used to calculate \code{class}
#' @param btn_type button style, choices are \code{'button'} or \code{'link'}
#' @param width,class,style,... passed to 'HTML' tag
#' @return A 'HTML' button tag
#' @export
run_analysis_button <- function(
    label = "Run analysis (Ctrl+Enter)",
    icon = NULL, width = NULL, type = "primary",
    btn_type = c("button", "link"), class = "", style = "", ...) {
  if (length(type) > 1) {
    type <- type[[1]]
  }

  args <- list(...)
  width <- c(width, "auto")[[1]]
  style <- paste0("width: ", shiny::validateCssUnit(width),
                  ";", style)

  btn_type <- match.arg(btn_type)

  if (btn_type == "button") {
    stopifnot(
      "type must be in 'default', 'primary', 'info', 'success', 'warning', 'danger'" =
        length(type) == 0 ||
        type[[1]] %in% c("default", "primary", "info",
                         "success", "warning", "danger")
    )
    class <- shidashi::combine_html_class(
      sprintf("btn btn-%s rave-button %s", type, class))

    shiny::tags$button(
      class = class,
      style = style,
      type = "button",
      "rave-action" = '{"type": "run_analysis"}',
      list(shidashi::as_icon(icon), label),
      ...
    )
  } else {

    class <- shidashi::combine_html_class("rave-button", class)
    shiny::tags$a(
      href = "#",
      class = class,
      style = style,
      "rave-action" = '{"type": "run_analysis"}',
      ...,
      list(label, shidashi::as_icon(icon))
    )
  }


}


# shiny_cache moved to R/deprecated.R




