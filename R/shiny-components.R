
column_md <- function(width, ..., class = ""){
  if (!is.numeric(width) || (width < 1) || (width > 12)) {
    stop("column width must be between 1 and 12")
  }
  colClass <- paste0("col-md-", width)
  colClass <- dipsaus::combine_html_class(colClass, class)
  shiny::div(class = colClass, ...)
}


#' Simple input-output layout
#' @description Provides simple layout, with inputs on the left, and outputs
#' on the right. Only useful in \code{'shidashi'} framework.
#' @param input_ui the 'HTML' tags for the inputs
#' @param output_ui the 'HTML' tags for the outputs
#' @param input_width width of inputs, must be an integer from 1 to 11
#' @param container_fixed whether the maximum width of the container should be
#' fixed; default is no
#' @param container_style additional 'CSS' style of the container
#' @param scroll whether to stretch the container to full-heights and
#' scroll the input and output separately.
#' @return 'HTML' tags
#'
#' @examples
#'
#' library(shiny)
#' library(ravedash)
#'
#' simple_layout(
#'   input_ui = list(
#'     ravedash::input_card(
#'       title = "Data Selection",
#'       "Add inputs here"
#'     )
#'   ),
#'   output_ui = list(
#'     ravedash::output_card(
#'       title = "Result A",
#'       "Add outputs here"
#'     )
#'   )
#' )
#'
#'
#' @export
simple_layout <- function(
  input_ui,
  output_ui,
  input_width = 4L,
  container_fixed = FALSE,
  container_style = NULL,
  scroll = FALSE
) {

  input_width <- as.integer(input_width)
  if(input_width < 1 || input_width > 11){
    stop("`input_width` must be an integer between 2 to 11")
  }
  container_class <- if(container_fixed){ "container" } else { "container-fluid" }
  input_ui <- substitute(input_ui)
  output_ui <- substitute(output_ui)

  if(scroll){
    inner_class <- 'row screen-height overflow-y-scroll fancy-scroll-y'
  } else {
    inner_class <- 'row'
  }

  expr <- bquote(shiny::div(
    class = .(container_class),
    style = .(container_style),
    shiny::fluidRow(
      shiny::div(
        class = .(sprintf("col-md-%d", input_width)),
        shiny::div(
          class = .(inner_class),
          shiny::column(
            width = 12L,
            .(input_ui)
          )
        )
      ),
      shiny::div(
        class = .(sprintf("col-md-%d", 12L-input_width)),
        shiny::div(
          class = .(inner_class),
          shiny::column(
            width = 12L,
            .(output_ui)
          )
        )
      )
    )
  ))

  eval(expr, envir = parent.frame())


}

#' @name group_box
#' @title Group input elements into a box with title
#' @description Only works in template framework provided by \code{'shidashi'}
#' package, see \code{\link[shidashi]{use_template}}
#' @param title the box title
#' @param ... elements to be included or to be passed to other methods
#' @param class additional class of the box
#' @param wrap,direction see \code{\link[shidashi]{flex_container}}
#' @return A 'HTML' tag
#' @examples
#'
#'
#' library(shiny)
#' library(shidashi)
#' library(ravedash)
#'
#' group_box(
#'   title = "Analysis Group A",
#'   selectInput("a", "Condition", choices = c("A", "B")),
#'   sliderInput("b", "Time range", min = 0, max = 1, value = c(0,1))
#' )
#'
#' flex_group_box(
#'   title = "Project and Subject",
#'   flex_item( "Some input 1" ),
#'   flex_item( "Some input 2" ),
#'   flex_break(),
#'   flex_item( "Some input in new line" )
#' )
#'
#' @export
group_box <- function(title, ..., class = NULL){
  class <- dipsaus::combine_html_class(class, "group-input-box")
  shiny::div(class = class, ..., title = title)
}

#' @rdname group_box
#' @export
flex_group_box <- function(title, ..., class = NULL, wrap = "wrap",
                      direction = 'row'){
  class <- dipsaus::combine_html_class(class, "group-input-box")
  shidashi::flex_container(class = class, wrap = wrap, direction = direction, ..., title = title)
}


#' Create a badge widget located at card header
#' @param text inner text content of the badge
#' @param class additional 'HTML' class of the badge; for
#' \code{set_card_badge}, this is the class selector of the badge that is
#' to be changed
#' @param ... additional 'HTML' tag attributes
#' @param id the badge 'HTML' ID to be changed, will be enclosed with
#' session namespace \code{session$ns(id)} automatically.
#' @param add_class,remove_class add or remove class
#' @param session shiny session
#' @examples
#'
#' library(ravedash)
#'
#' # UI: a Bootstrap badge with green background
#' card_badge("Ready", class = "bg-green rave-output-status")
#'
#' # server
#' server <- function(input, output, session) {
#'
#'   safe_observe({
#'
#'     # ... check if the inputs have changed
#'
#'     set_card_badge(
#'       class = "rave-output-status",
#'       text = "Refresh needed",
#'       add_class = "bg-yellow",
#'       remove_class = "bg-green"
#'     )
#'
#'   })
#'
#' }
#'
#' @export
card_badge <- function(text = NULL, class = NULL, ...){
  if(!length(text) || nchar(text) == 0){
    text <- ''
  }
  class <- dipsaus::combine_html_class("right badge rave-card-badge", class)
  shiny::span(class=class, text, ...)
}

#' @rdname card_badge
#' @export
card_recalculate_badge <- function(
    text = "Recalculate needed", class = NULL, ...) {
  card_badge(
    text = text,
    class = dipsaus::combine_html_class(
      "btn btn-default rave-button rave-button-autorecalculate rave-output-status bg-yellow",
      class
    ),
    `rave-action`='{"type": "run_analysis"}',
    ...
  )
}

#' @rdname card_badge
#' @export
enable_recalculate_badge <- function(
    text = "Recalculate needed", ...) {
  set_card_badge(
    text = text, class = "rave-output-status",
    add_class = "btn btn-default rave-button rave-button-autorecalculate bg-yellow",
    remove_class = "bg-green"
  )
}

#' @rdname card_badge
#' @export
disable_recalculate_badge <- function(
    text = "Up-to-date", ...) {
  set_card_badge(
    text = text, class = "rave-output-status",
    remove_class = "btn btn-default rave-button rave-button-autorecalculate bg-yellow",
    add_class = "bg-green"
  )
}

#' @rdname card_badge
#' @export
set_card_badge <- function(
    id = NULL, class = NULL,
    text = NULL, add_class = NULL, remove_class = NULL,
    session = shiny::getDefaultReactiveDomain()) {
  if(!length(id) && !length(class)) { return() }
  if( is.null(session) ) { return() }

  if(length(id)) {
    selector <- sprintf("#%s.rave-card-badge", session$ns(id))
  } else {
    selector <- sprintf(".%s.rave-card-badge", class)
  }

  if(length(text)) {
    session$sendCustomMessage(
      "shidashi.set_html",
      message = list(
        selector = selector,
        content = text
      )
    )
  }
  if(length(add_class)) {
    add_class <- trimws(unlist(strsplit(add_class, " ")))
    lapply(add_class, function(cls) {
      session$sendCustomMessage(
        "shidashi.add_class",
        message = list(
          selector = selector,
          class = cls
        )
      )
    })
  }
  if(length(remove_class)) {
    remove_class <- trimws(unlist(strsplit(remove_class, " ")))
    remove_class <- remove_class[!remove_class %in% "rave-output-status"]
    lapply(remove_class, function(cls) {
      session$sendCustomMessage(
        "shidashi.remove_class",
        message = list(
          selector = selector,
          class = cls
        )
      )
    })
  }
  invisible()
}






