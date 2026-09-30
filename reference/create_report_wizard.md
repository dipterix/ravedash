# Create report wizard to be used within the interactive modules

Create report wizard to be used within the interactive modules

## Usage

``` r
create_report_wizard(
  pipeline,
  session = shiny::getDefaultReactiveDomain(),
  env = parent.frame()
)
```

## Arguments

- pipeline:

  `ravepipeline` pipeline

- session:

  shiny session

- env:

  environment used to register the wizard inputs with shidashi, so that
  agents can choose and generate reports; default is the calling frame,
  typically the module server

## Value

A list of functions: `launch` with argument `subject` to be called when
users want to pop up a wizard allowing users to choose reports;
`generate` with arguments `subject` and `report_names` to generate
reports
