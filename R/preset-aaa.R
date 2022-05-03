#' @name rave-ui-preset
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
#' @param pipeline_repository the pipeline name that represents the 'RAVE'
#' repository from functions such as \code{\link[raveio]{prepare_subject_bare}},
#' \code{\link[raveio]{prepare_subject_with_epoch}}, and
#' \code{\link[raveio]{prepare_subject_power}}
#' @param max_components maximum number of components for compound inputs
#' @param baseline_choices the possible approaches to calculate baseline
#' @param baseline_along_choices the units of baseline
#' @param settings_entries used when importing pipelines, pipeline variable
#' names to be included or excluded, depending on \code{fork_mode}
#' @param fork_mode \code{'exclude'} (default) or \code{'include'}; in
#' \code{'exclude'} mode, \code{settings_entries} will be excluded from the
#' pipeline settings; in \code{'include'} mode, only \code{settings_entries}
#' can be imported.
#' @title Preset reusable 'RAVE' front-end elements
NULL
