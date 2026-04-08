.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  assign('.shiny_components', value = fastmap::fastmap(), envir = ns)
  try({
    dipsaus::registerInputBinding(
      fname = "pickerInput", pkg = "shinyWidgets",
      shiny_binding = "shinyWidgets.pickerInput",
      update_function = "shinyWidgets::updatePickerInput",
      quiet = TRUE)
  }, silent = TRUE)

  options("ravedash.urls" = dipsaus::fastmap2())

}


.onDetach <- function(libpath) {
  # No-op: session cleanup is handled by shidashi
}

