#' @export
input_card <- function(title, ...,
                       class_body = "padding-10",
                       class_foot = "padding-10",
                       href = "#", tools = NULL,
                       footer = NULL,
                       toggle_advanced = FALSE){
  all_tools = list(
    shidashi::card_tool(widget = "link", href = href, icon = shiny_icons$help),
    shidashi::card_tool(widget = "collapse"),
    tools
  )
  if(toggle_advanced){
    footer <- shiny::tagList(
      footer,
      shiny::tags$small(
        class = "fill-width display-block",
        shiny::a(href = "#", class = "toggle-advance-options float-right", "Show/Hide advanced options")
      )
    )
  }
  shidashi::card(title = title, ..., tools = all_tools, class_body = class_body, class_foot = class_foot, footer = footer)
}

#' @export
output_card <- function(title, ..., class_body = "padding-10",
                        class_foot = "padding-10", href = "#", tools = NULL){
  all_tools = list(
    shidashi::card_tool(widget = "link", href = href, icon = shiny_icons$help),
    shidashi::card_tool(widget = "collapse"),
    shidashi::card_tool(widget = "maximize"),
    tools
  )
  shidashi::card(title = title, ..., tools = all_tools, class_body = class_body, class_foot = class_foot)
}
