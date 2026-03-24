# Create report wizard to be used within the interactive modules

Create report wizard to be used within the interactive modules

## Usage

``` r
create_report_wizard(pipeline, session = shiny::getDefaultReactiveDomain())
```

## Arguments

- pipeline:

  `ravepipeline` pipeline

- session:

  shiny session

## Value

A list of functions: `launch` with argument `subject` to be called when
users want to pop up a wizard allowing users to choose reports;
`generate` with arguments `subject` and `report_names` to generate
reports
