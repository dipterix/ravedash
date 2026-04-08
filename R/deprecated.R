
#' Obtain caching object for current run-time shiny session
#' @description
#' \strong{Deprecated.} This function is no longer used by RAVE modules.
#'
#' Cache small objects such as inputs or configurations.
#' @param namespace characters, usually the module ID
#' @param session shiny interactive context domain
#' @returns A caching object. The caching object is identical within the same
#' context and namespace.
#' @export
shiny_cache <- function(namespace, session = shiny::getDefaultReactiveDomain()) {
  .Deprecated(
    msg = "`shiny_cache()` is deprecated and will be removed in a future release."
  )
  if (missing(namespace) || length(namespace) != 1) {
    stop(
      "`session_cache`: `namespace` (i.e. module ID) must be explicitly specified."
    )
  }
  if (!is.environment(session)) {
    warning(
      "`session_cache`: session must be a shiny reactive context. Are you running shiny application?"
    )
    return(cache_mem2())
  }
  module_cache <- session$userData$ravedash$module_cache
  if (!inherits(module_cache, "fastmap2")) {
    module_cache <- dipsaus::fastmap2()
    if (inherits(session$userData$ravedash, "fastmap2")) {
      session$userData$ravedash$module_cache <- module_cache
    }
  }

  if (!inherits(module_cache[[namespace]], "cache_mem2")) {
    module_cache[[namespace]] <- cache_mem2()
  }

  return(module_cache[[namespace]])

}
