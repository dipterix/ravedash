#' Check whether repository read from pipeline has been updated
#' @description
#' Helper function to check if new pipeline repository is loaded; see 'Examples'
#'
#' @param component_container A \code{'RAVEShinyComponentContainer'} instance
#' @param pipeline A \code{\link[ravepipeline]{pipeline}} instance
#' @param repository_class Expected repository class; default is arbitrary
#' @param session A shiny session
#' @returns Logical \code{TRUE} or \code{FALSE} is new repository is loaded
#' @examples
#'
#'
#' \dontrun{
#'
#' shiny::bindEvent(
#'   ravedash::safe_observe({
#'     if (!ravedash::on_new_repository_loaded()) {
#'       return()
#'     }
#'
#'     # Handle event when new repository is loaded
#'
#'   }, priority = 1001),
#'   ravedash::watch_data_loaded(),
#'   ignoreNULL = FALSE,
#'   ignoreInit = FALSE
#' )
#'
#'
#' }
#'
#'
#'
#' @export
check_new_repository_loaded <- function(
    component_container, pipeline, repository_class = NA,
    session = shiny::getDefaultReactiveDomain()) {

  loaded_flag <- watch_data_loaded(session = session)
  if (!loaded_flag) {
    observer_early_terminate(signal = TRUE)
    return(FALSE)
  }
  new_repository <- pipeline$read("repository",
                                  ifnotfound = NULL)
  if (is.null(new_repository)) {
    observer_early_terminate(
      message = "Repository not loaded from the pipeline",
      level = "warning"
    )
    return(FALSE)
  }
  if (!is.na(repository_class) && !inherits(new_repository, repository_class)) {
    observer_early_terminate(
      message = sprintf(
        "Repository read from the pipeline, but it is not an instance of `%s`; abort initialization.",
        repository_class
      ),
      level = "warning"
    )
    return(FALSE)
  }

  # check if the repository has the same subject as current one
  old_repository <- component_container$data$repository

  if (!is.null(old_repository) && !attr(loaded_flag, "force") &&
      identical(old_repository$signature, new_repository$signature) &&
      (is.na(repository_class) || inherits(old_repository, repository_class))
  ) {
    observer_early_terminate(
      message = "The repository data remain unchanged; skip initialization.",
      level = "debug"
    )
    return(FALSE)
  }
  logger(
    "Repository read from the pipeline; initializing the module UI",
    level = "debug"
  )

  # Reset preset UI & data
  component_container$reset_data()
  component_container$data$repository <- new_repository
  component_container$initialize_with_new_data()

  return(new_repository)
}

observer_early_terminate <- function(
    message = NULL, call = NULL, level = "debug", signal = TRUE) {
  cond <- structure(
    list(message = as.character(message), call = call, level = level),
    class = c("observer_early_terminate", "condition")
  )
  if (signal) {
    signalCondition(cond)
  }
  invisible(cond)
}
