# Creates a container for preset components

Creates a container for preset components

## Usage

``` r
new_rave_shiny_component_container(
  module_id,
  pipeline_name,
  pipeline_path = ravepipeline::pipeline_find(pipeline_name),
  settings_file = "settings.yaml"
)
```

## Arguments

- module_id:

  'RAVE' module ID

- pipeline_name:

  the name of pipeline to run

- pipeline_path:

  path of the pipeline

- settings_file:

  the settings file of the pipeline, usually stores the pipeline input
  information; default is `"settings.yaml"`

## Value

A `'RAVEShinyComponentContainer'` instance

## Examples

``` r

f <- tempfile()
dir.create(f, showWarnings = FALSE, recursive = TRUE)
file.create(file.path(f, "settings.yaml"))
#> [1] TRUE

container <- new_rave_shiny_component_container(
  module_id = "module_power_phase_coherence",
  pipeline_name = "power_phase_coherence_pipeline",
  pipeline_path = f
)

loader_project <- presets_loader_project()
loader_subject <- presets_loader_subject()

container$add_components(
  loader_project, loader_subject
)

```
