.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  assign('.sessions', value = fastmap::fastmap(), envir = ns)
  assign('.shiny_components', value = fastmap::fastmap(), envir = ns)
  try({
    dipsaus::capture_expr({
      dipsaus::registerInputBinding(
        fname = "pickerInput", pkg = "shinyWidgets",
        shiny_binding = "shinyWidgets.pickerInput",
        update_function = "shinyWidgets::updatePickerInput")
    })
  }, silent = TRUE)
}

#' @export
.Last.lib <- function(libpath){
  tryCatch({
    sess <- get0(x = '.sessions', ifnotfound = NULL)
    if(inherits(sess, 'fastmap')){
      li <- sess$as_list()
      sess$reset()
      lapply(li, function(item) {
        # Finalize item
      })
    }
  }, error = function(e){
    warning(e)
  })
}
