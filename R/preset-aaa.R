#' @name rave-ui-preset
#' @title Preset reusable front-end components for 'RAVE' modules
#' @description For examples and use cases, please check
#' \code{\link{new_rave_shiny_component_container}}.
#' @param id input or output ID of the element; this ID will be prepended with
#' module namespace
#' @param varname variable name(s) in the module's settings file
#' @param label readable label(s) of the element
#' @param height height of the element
#' @param loader_project_id the ID of \code{presets_loader_project} if
#' different to the default
#' @param loader_subject_id the ID of \code{presets_loader_subject} if
#' different to the default
#' @param loader_reference_id the ID of \code{presets_loader_reference} if
#' different to the default
#' @param loader_electrodes_id the ID of \code{presets_loader_electrodes} if
#' different to the default
#' @param import_setup_id the ID of \code{presets_import_setup_native} if
#' different to the default
#' @param pipeline_repository the pipeline name that represents the 'RAVE'
#' repository from functions such as
#' \code{\link[ravecore]{prepare_subject_bare}},
#' \code{\link[ravecore]{prepare_subject_with_epochs}}, and
#' \code{\link[ravecore]{prepare_subject_power}}
#' @param max_components maximum number of components for compound inputs
#' @param baseline_choices the possible approaches to calculate baseline
#' @param baseline_along_choices the units of baseline
#' @param settings_entries used when importing pipelines, pipeline variable
#' names to be included or excluded, depending on \code{fork_mode}
#' @param fork_mode \code{'exclude'} (default) or \code{'include'}; in
#' \code{'exclude'} mode, \code{settings_entries} will be excluded from the
#' pipeline settings; in \code{'include'} mode, only \code{settings_entries}
#' can be imported.
#' @param from_module which module to extract input settings
#' @param project_varname,subject_varname variable names that should be
#' extracted from the settings file
#' @param mode whether to create new reference, or simply to choose from
#' existing references
#' @param checks whether to check if subject has been applied with 'Notch'
#' filters or 'Wavelet'; default is both.
#' @param start_simple whether to start in simple view and hide optional inputs
#' @param multiple whether to allow multiple inputs
#' @param allow_new whether to allow new subject to be created; ignored when
#' checks exist
#' @param allow_stitch whether to allow stitching the events
#' @param env environment in which the preset is created; default is the
#' calling frame, typically the module environment where the module scripts
#' are sourced. \pkg{shidashi} looks up the module's input registry from this
#' environment, so that agents can query and update the preset inputs
#' @param hint hints for agents (see \code{shidashi::input_hint_classes}):
#' whether they ask the user for an input before loading data or running the
#' analysis, keep its default, or leave it alone.  Each preset has its own
#' defaults (for example, loader inputs are \code{"loader_mandatory"}); give
#' one unnamed value for every input of the preset, or values named by input
#' ID to override some of them, for example
#' \code{c(loader_epoch_name__trial_starts = "loader_optional")}
#' @param ... ignored, typically reserved for obsolete arguments
#' @returns A \code{'RAVEShinyComponent'} instance.
#' @seealso \code{\link{new_rave_shiny_component_container}}
NULL

# The hint of each input a preset registers with `shidashi`. `defaults` maps
# the preset's sub-element names (`main` for its main input) to hints; the
# caller's `hint` overrides them: one unnamed value for every input, or
# values named by input ID. Inputs without a hint get "no_hint".
preset_hint_getter <- function(comp, hint, defaults = c(main = "no_hint")) {
  hint <- unlist(hint)
  force(defaults)
  function(sub = "main") {
    if (length(hint) == 1L && is.null(names(hint))) {
      return(hint[[1]])
    }
    input_id <- if (identical(sub, "main")) {
      comp$get_sub_element_id(with_namespace = FALSE)
    } else {
      comp$get_sub_element_id(sub, with_namespace = FALSE)
    }
    if (length(hint) && input_id %in% names(hint)) {
      return(hint[[input_id]])
    }
    if (sub %in% names(defaults)) defaults[[sub]] else "no_hint"
  }
}

