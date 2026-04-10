# Preset reusable front-end components for 'RAVE' modules

For examples and use cases, please check
[`new_rave_shiny_component_container`](https://dipterix.org/ravedash/reference/new_rave_shiny_component_container.md).

## Usage

``` r
presets_analysis_electrode_selector2(
  id = "electrode_text",
  varname = "analysis_electrodes",
  label = "Select Electrodes",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  pipeline_repository = "repository",
  start_simple = FALSE,
  multiple = TRUE
)

presets_analysis_ranges(
  id = "analysis_ranges",
  varname = "analysis_ranges",
  label = "Configure Analysis",
  pipeline_repository = "repository",
  max_components = 2
)

presets_baseline_choices(
  id = "baseline_choices",
  varname = "baseline",
  label = "Baseline Settings",
  pipeline_repository = "repository",
  baseline_choices = c("Decibel", "% Change Power", "% Change Amplitude",
    "z-score Power", "z-score Amplitude"),
  baseline_along_choices = c("Per frequency, trial, and electrode", "Across electrode",
    "Across trial", "Across trial and electrode")
)

presets_condition_groups(
  id = "condition_groups",
  varname = "condition_groups",
  label = "Create Condition Contrast",
  pipeline_repository = "repository"
)

presets_import_export_subject_pipeline(
  id = "im_ex_pipeline",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  pipeline_repository = "repository",
  settings_entries = c("loaded_electrodes", "epoch_choice", "epoch_choice__trial_starts",
    "epoch_choice__trial_ends", "reference_name"),
  fork_mode = c("exclude", "include")
)

presets_import_setup_blocks(
  id = "import_blocks",
  label = "Format & session blocks",
  import_setup_id = "import_setup",
  max_components = 5
)

presets_import_setup_native(
  id = "import_setup",
  label = "Select project & subject"
)

presets_loader_3dviewer(
  id = "loader_3d_viewer",
  height = "600px",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  loader_reference_id = "loader_reference_name",
  loader_electrodes_id = "loader_electrode_text",
  ...
)

presets_loader_3dviewer2(
  id = "loader_3d_viewer",
  height = "600px",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  loader_electrodes_id = "loader_electrode_text",
  ...
)

presets_loader_electrodes(
  id = "loader_electrode_text",
  varname = "loaded_electrodes",
  label = "Electrodes",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code"
)

presets_loader_epoch(
  id = "loader_epoch_name",
  varname = "epoch_choice",
  label = "Epoch and Trial Duration",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  allow_stitch = FALSE
)

presets_loader_project(
  id = "loader_project_name",
  varname = "project_name",
  label = "Project"
)

presets_loader_reference(
  id = "loader_reference_name",
  varname = "reference_name",
  label = "Reference name",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  mode = c("default", "create")
)

presets_loader_subject(
  id = "loader_subject_code",
  varname = "subject_code",
  label = "Subject",
  loader_project_id = "loader_project_name",
  checks = c("notch", "wavelet"),
  allow_new = FALSE
)

presets_loader_subject_only(
  id = "loader_subject_code",
  varname = "subject_code",
  label = "Subject",
  multiple = FALSE
)

presets_loader_sync_project_subject(
  id = "loader_sync_project_subject",
  label = "Sync subject from most recently loaded",
  varname = "loader_sync_project_subject",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  from_module = NULL,
  project_varname = "project_name",
  subject_varname = "subject_code"
)
```

## Arguments

- id:

  input or output ID of the element; this ID will be prepended with
  module namespace

- varname:

  variable name(s) in the module's settings file

- label:

  readable label(s) of the element

- loader_project_id:

  the ID of `presets_loader_project` if different to the default

- loader_subject_id:

  the ID of `presets_loader_subject` if different to the default

- pipeline_repository:

  the pipeline name that represents the 'RAVE' repository from functions
  such as
  [`prepare_subject_bare`](http://rave.wiki/ravecore/reference/prepare_subject_bare.md),
  [`prepare_subject_with_epochs`](http://rave.wiki/ravecore/reference/prepare_subject_with_epochs.md),
  and
  [`prepare_subject_power`](http://rave.wiki/ravecore/reference/prepare_subject_with_epochs.md)

- start_simple:

  whether to start in simple view and hide optional inputs

- multiple:

  whether to allow multiple inputs

- max_components:

  maximum number of components for compound inputs

- baseline_choices:

  the possible approaches to calculate baseline

- baseline_along_choices:

  the units of baseline

- settings_entries:

  used when importing pipelines, pipeline variable names to be included
  or excluded, depending on `fork_mode`

- fork_mode:

  `'exclude'` (default) or `'include'`; in `'exclude'` mode,
  `settings_entries` will be excluded from the pipeline settings; in
  `'include'` mode, only `settings_entries` can be imported.

- import_setup_id:

  the ID of `presets_import_setup_native` if different to the default

- height:

  height of the element

- loader_reference_id:

  the ID of `presets_loader_reference` if different to the default

- loader_electrodes_id:

  the ID of `presets_loader_electrodes` if different to the default

- ...:

  ignored, typically reserved for obsolete arguments

- allow_stitch:

  whether to allow stitching the events

- mode:

  whether to create new reference, or simply to choose from existing
  references

- checks:

  whether to check if subject has been applied with 'Notch' filters or
  'Wavelet'; default is both.

- allow_new:

  whether to allow new subject to be created; ignored when checks exist

- from_module:

  which module to extract input settings

- project_varname, subject_varname:

  variable names that should be extracted from the settings file

## Value

A `'RAVEShinyComponent'` instance.

## See also

[`new_rave_shiny_component_container`](https://dipterix.org/ravedash/reference/new_rave_shiny_component_container.md)
