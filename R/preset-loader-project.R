
#' @name rave-ui-preset
#' @param id input or output ID of the element; this ID will be prepended with
#' module namespace
#' @param varname,varnames variable name(s) in the module's settings file
#' @param label,labels readable label(s) of the element
#' @param height height of the element
#' @param loader_project_id the ID of \code{presets_loader_project} if
#' different to the default
#' @param loader_subject_id the ID of \code{presets_loader_subject} if
#' different to the default
#' @param loader_reference_id the ID of \code{presets_loader_reference} if
#' different to the default
#' @param loader_electrodes_id the ID of \code{presets_loader_electrodes} if
#' different to the default
#' @title Preset reusable 'RAVE' front-end elements
#' @export
presets_loader_project <- function(
  id = "loader_project_name", varname = "project_name",
  label = "Project"
){

  comp <- RAVEShinyComponent$new(id = id, varname = varname)

  comp$ui_func <- function(id, value, depends){
    choices <- raveio::get_projects(refresh = FALSE)
    shiny::selectInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = value %OF% choices,
      multiple = FALSE
    )
  }
  comp$add_rule(function(value){
    if(length(value) != 1 || is.na(value)){
      return("Missing project name. Please choose one.")
    }
    project <- raveio::as_rave_project(value, strict = FALSE)
    if(!dir.exists(project$path)){
      return(raveio::glue("Cannot find path to project `{value}`"))
    }
    return(NULL)
  })

  comp

}
