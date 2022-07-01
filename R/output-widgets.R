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
