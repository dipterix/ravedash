#' @export
input_card <- function(title, ...,
                       class_body = "padding-5",
                       class_foot = "padding-5", href = "#", tools = NULL){
  all_tools = list(
    shidashi::card_tool(widget = "link", href = href, icon = rave::shiny_icons$help),
    shidashi::card_tool(widget = "collapse"),
    tools
  )
  shidashi::card(title = title, ..., tools = all_tools, class_body = class_body, class_foot = class_foot)
}

#' @export
output_card <- function(title, ..., class_body = "padding-5",
                        class_foot = "padding-5", href = "#", tools = NULL){
  all_tools = list(
    shidashi::card_tool(widget = "link", href = href, icon = rave::shiny_icons$help),
    shidashi::card_tool(widget = "collapse"),
    shidashi::card_tool(widget = "maximize"),
    tools
  )
  shidashi::card(title = title, ..., tools = all_tools, class_body = class_body, class_foot = class_foot)
}
