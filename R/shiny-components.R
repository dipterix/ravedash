
#' @export
column_md <- function(width, ..., class = ""){
  if (!is.numeric(width) || (width < 1) || (width > 12)) {
    stop("column width must be between 1 and 12")
  }
  colClass <- paste0("col-md-", width)
  colClass <- shidashi:::combine_class(colClass, class)
  shiny::div(class = colClass, ...)
}

#' @export
simple_layout <- function(
  input_ui,
  output_ui,
  input_width = 4L,
  container_fixed = FALSE,
  container_style = NULL
) {

  input_width <- as.integer(input_width)
  if(input_width < 1 || input_width > 11){
    stop("`input_width` must be an integer between 2 to 11")
  }
  container_class <- if(container_fixed){ "container" } else { "container-fluid" }
  input_ui <- substitute(input_ui)
  output_ui <- substitute(output_ui)

  expr <- bquote(shiny::div(
    class = .(container_class),
    style = .(container_style),
    shiny::fluidRow(
      shiny::div(
        class = .(sprintf("col-md-%d", input_width)),
        .(input_ui)
      ),
      shiny::div(
        class = .(sprintf("col-md-%d", 12L-input_width)),
        .(output_ui)
      )
    )
  ))

  eval(expr, envir = parent.frame())


}

#' @export
group_box <- function(title, ..., class = NULL){
  class <- dipsaus::combine_html_class(class, "group-input-box")
  shiny::div(class = class, ..., title = title)
}

#' @export
flex_group_box <- function(title, ..., class = NULL, use_flex = TRUE, wrap = "wrap",
                      direction = 'row'){
  class <- dipsaus::combine_html_class(class, "group-input-box")
  shidashi::flex_container(class = class, wrap = wrap, direction = direction, ..., title = title)
}


#' @export
flex_item2 <- function(..., size = "1", class = NULL){
  class <- dipsaus::combine_html_class(class, "fill-width padding-5")
  shidashi::flex_item(
    flex = as.character(size),
    class = class,
    ...
  )
}

#' @export
flex_new_line <- function(..., class = NULL){
  class <- dipsaus::combine_html_class(class, "flex-break")
  shiny::div(class = class, ...)
}
