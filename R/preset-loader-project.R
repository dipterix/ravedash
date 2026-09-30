

#' @rdname rave-ui-preset
#' @export
presets_loader_project <- function(
  id = "loader_project_name", varname = "project_name",
  label = "Project", env = parent.frame()
) {

  comp <- RAVEShinyComponent$new(id = id, varname = varname)

  # shidashi::register_input calls .register_input
  # which exists when presets_loader_project() is called, not when ui_func is called
  parse_env <- env

  comp$ui_func <- function(id, value, depends) {
    choices <- ravecore::get_projects(refresh = FALSE)
    shidashi::register_input(
      bquote(shiny::selectInput(
        inputId = .(id),
        label = .(label),
        choices = .(choices),
        selected = .(value %OF% choices),
        multiple = FALSE
      )),
      inputId = comp$get_sub_element_id(with_namespace = FALSE),
      update = "shiny::updateSelectInput(value=selected)",
      description = "RAVE project name to load data from.",
      env = parse_env, 
      quoted = TRUE
    )
  }
  comp$add_rule(function(value) {
    if (length(value) != 1 || is.na(value)) {
      return("Missing project name. Please choose one.")
    }
    project <- ravecore::as_rave_project(value, strict = FALSE)
    if (!dir.exists(project$path)) {
      return(ravepipeline::glue("Cannot find path to project `{value}`"))
    }
    return(NULL)
  })

  comp

}


