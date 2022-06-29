#' @export
output_widget <- function(
  outputId, icon = NULL,
  type = c("standalone", "download", 'action', 'custom'),
  class = NULL, inputId = NULL, ...
) {
  type <- match.arg(type)

  if(is.null(icon)) {
    icon <- switch (
      type,
      standalone = shiny_icons$external_link,
      download = shiny_icons$download,
      {
        shiny_icons$puzzle
      }
    )
  }

  if(type %in% c('actionbutton', 'download')) {
    if(length(inputId) != 1) {
      inputId <- sprintf("%s__%s", outputId, type)
    }
  }

  class <- dipsaus::combine_html_class(
    "ravedash-output-widget",
    switch (
      type,
      actionbutton = "action-button shiny-bound-input",
      download = "shiny-download-link",
      {NULL}
    ),
    class
  )

  args <- list(
    href = "#",
    class = class,
    `data-type` = type,
    `data-target` = outputId,
    icon,
    ...
  )
  args$id <- inputId
  if(type %in% c("download")) {
    args$target <- "_blank"
    args$download <- ""
    args$href <- ""
  }

  do.call(shiny::tags$a, args)
}

#' @export
output_widget_container <- function(
  ..., .list = list(), class = NULL
) {
  args = c(
    list(
      class = dipsaus::combine_html_class("ravedash-output-widget-container", class),
      ...
    ),
    .list
  )
  do.call(shiny::div, args)
}

#' @export
render_output <- function(
    outputId, renderer, expr, ..., quoted = FALSE,
    .env = parent.frame(),
    .options = list(),
    .export_type = c("none", "plot", 'table', 'widget', '3dviewer'),
    .export_name = outputId,
    .export_ext = "",
    .export_prefunc = NULL,
    .export_postfunc = NULL,
    .session = shiny::getDefaultReactiveDomain()) {
  .export_type <- match.arg(.export_type)
  if(!quoted) {
    expr <- substitute(expr)
  }
  renderer <- substitute(renderer)

  re <- eval(as.call(list(
    renderer, expr, ..., quoted = FALSE)), envir = .env)
  .session$output[[outputId]] <- re

  render_f <- dipsaus::new_function2(body = expr, quote_type = "quote", env = .env)

  download_id <- sprintf("%s__download", outputId)
  get_fname <- function() {
    sprintf("[rave-export-%s]%s%s",
            strftime(Sys.time(), "%y%m%d-%H%M%S"),
            .export_name, .export_ext)
  }

  prefunc <- function() {
    dipsaus::shiny_alert2(
      "Exporting Widget",
      icon = "info",
      text = "Please wait...",
      auto_close = FALSE,
      buttons = FALSE
    )
    do.call(on.exit, list(quote({
      dipsaus::close_alert2()
    }), add = TRUE, after = TRUE),
    envir = parent.frame())
  }


  if(.export_type == "plot") {
    if(!is.function(.export_prefunc)) {
      .export_prefunc <- function(con) {
        grDevices::pdf(file = con, width = 12, height = 6.75,
                       useDingbats = FALSE, onefile = TRUE)
      }
    }
    if(!is.function(.export_postfunc)) {
      .export_postfunc <- function() {
        dev.off()
      }
    }
    if(!nchar(.export_ext)) {
      .export_ext <- ".pdf"
    }
    .session$output[[download_id]] <- shiny::downloadHandler(
      filename = get_fname,
      content = function(con) {
        prefunc()
        .export_prefunc(con)
        on.exit({
          .export_postfunc()
        }, add = TRUE, after = FALSE)

        shiny::isolate(render_f())
      }
    )
  } else if (.export_type == "widget") {

    .session$output[[download_id]] <- shiny::downloadHandler(
      filename = get_fname,
      content = function(con) {
        prefunc()
        if(is.function(.export_prefunc)) {
          .export_prefunc(con)
        }
        if(is.function(.export_postfunc)) {
          on.exit({
            .export_postfunc()
          }, add = TRUE, after = FALSE)
        }

        widget <- shiny::isolate(render_f())
        htmlwidgets::saveWidget(widget = widget, file = con, selfcontained = TRUE, title = .export_name)
      }
    )

  } else if (.export_type == "table") {
    .export_ext <- ".csv"
    .session$output[[download_id]] <- shiny::downloadHandler(
      filename = get_fname,
      content = function(con) {
        prefunc()
        tbl <- shiny::isolate(render_f())
        tbl <- as.data.frame(tbl)
        write.csv(x = tbl, file = con)
      }
    )
  } else if (.export_type == '3dviewer') {
    .export_ext <- ".zip"
    .session$output[[download_id]] <- shiny::downloadHandler(
      filename = get_fname,
      content = function(con) {
        prefunc()
        widget <- shiny::isolate(render_f())
        tf <- tempfile()
        on.exit({
          unlink(tf, recursive = TRUE)
        }, add = TRUE)
        threeBrain::save_brain(widget = widget, directory = tf,
                               as_zip = TRUE, title = .export_name)
        file.copy(file.path(tf, "compressed.zip"), con)
      }
    )
  }



  register_output_options(
    outputId,
    .opt = .options,
    session = .session,
    extras = list(render_expression = expr,
                  export_type = .export_type)
  )

  return(re)
}

get_output <- function(outputId, session = shiny::getDefaultReactiveDomain()) {

  module_id <- session$ns(NULL)
  # make sure we are working on the root scope
  session <- session$rootScope()

  # get module information
  if(!is.null(module_info)) {
    module_info <- get_active_module_info(session = session)
    module_id <- module_info$id
  }

  # get renderer's information
  reactive_handlers <- get_default_handlers(session = session)
  output_options <- reactive_handlers$output_options
  ns <- shiny::NS(module_id)
  outputId_full <- ns(outputId)
  render_function <- session$getOutput(outputId_full)

  if(!is.function(render_function)) {
    stop("Cannot find render function for output: ", outputId)
  }

  # get output function
  ui_function <- attr(render_function, "outputFunc")

  # get output options
  output_opt <- as.list(output_options[[outputId_full]])

  list(
    namespace = ns(NULL),
    outputId = outputId,
    outputId_full = outputId_full,
    render_function = render_function,
    output_function = ui_function,
    output_args = output_opt$args,
    extras = output_opt$extras
  )

}
