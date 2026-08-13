# Check whether repository read from pipeline has been updated

Helper function to check if new pipeline repository is loaded; see
'Examples'

## Usage

``` r
check_new_repository_loaded(
  component_container,
  pipeline,
  repository_class = NA,
  session = shiny::getDefaultReactiveDomain()
)
```

## Arguments

- component_container:

  A `'RAVEShinyComponentContainer'` instance

- pipeline:

  A [`pipeline`](http://dipterix.org/ravepipeline/reference/pipeline.md)
  instance

- repository_class:

  Expected repository class; default is arbitrary

- session:

  A shiny session

## Value

Logical `TRUE` or `FALSE` is new repository is loaded

## Examples

``` r


if (FALSE) { # \dontrun{

shiny::bindEvent(
  ravedash::safe_observe({
    if (!ravedash::on_new_repository_loaded()) {
      return()
    }

    # Handle event when new repository is loaded

  }, priority = 1001),
  ravedash::watch_data_loaded(),
  ignoreNULL = FALSE,
  ignoreInit = FALSE
)


} # }


```
