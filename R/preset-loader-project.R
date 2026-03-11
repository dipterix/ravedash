

#' @rdname rave-ui-preset
#' @export
presets_loader_project <- function(
  id = "loader_project_name", varname = "project_name",
  label = "Project"
){

  comp <- RAVEShinyComponent$new(id = id, varname = varname)

  comp$ui_func <- function(id, value, depends){
    choices <- ravecore::get_projects(refresh = FALSE)
    shidashi::register_input(
      shiny::selectInput(
        inputId = id,
        label = label,
        choices = choices,
        selected = value %OF% choices,
        multiple = FALSE
      ),
      inputId = comp$get_sub_element_id(with_namespace = FALSE),
      update = "shiny::updateSelectInput(value=selected)",
      description = "RAVE project name to load data from."
    )
  }
  comp$add_rule(function(value){
    if(length(value) != 1 || is.na(value)){
      return("Missing project name. Please choose one.")
    }
    project <- ravecore::as_rave_project(value, strict = FALSE)
    if(!dir.exists(project$path)){
      return(ravepipeline::glue("Cannot find path to project `{value}`"))
    }
    return(NULL)
  })

  comp

}


