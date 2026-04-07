
#' Shiny plot output with minimum height and additional classes
#' @param outputId,width,height,... passed to \code{\link[shiny]{plotOutput}}
#' @param class additional 'HTML' class of the output wrapper
#' @param min_height minimum height of the image; default is 400 pixels
#' @returns A plot output element that can be included in a panel.
#' @examples
#'
#' plotOutput2("plot", class = "rounded overflow-hidden",
#'             min_height = 300)
#'
#'
#' @export
plotOutput2 <- function(outputId, class = NULL,
                        width = "100%", height = "100%", min_height = "400px", ...) {

  res <- shiny::plotOutput(
    outputId = outputId,
    height = width, width = height, ...
  )

  res$attribs$class <- shidashi::combine_html_class(res$attribs$class, class)

  res$attribs$style <- sprintf(
    "%s;min-height:%s",
    paste(res$attribs$style, collapse = ""),
    shiny::validateCssUnit(min_height)
  )

  res
}
