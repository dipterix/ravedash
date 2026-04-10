# Default module server function

Common shiny server function to enable modules that requires data loader
panel.

## Usage

``` r
module_server_common(
  module_id,
  check_data_loaded,
  ...,
  session = shiny::getDefaultReactiveDomain(),
  parse_env = NULL
)
```

## Arguments

- module_id:

  'RAVE' module ID

- check_data_loaded:

  a function that takes zero to one argument and must return either
  `TRUE` if data has been loaded or `FALSE` if loader needs to be open
  to load data.

- ...:

  ignored

- session:

  shiny session;
  [`init_app`](https://dipterix.org/shidashi/reference/init_app.html)
  must be called before this function so that the shidashi session
  registry is initialized.

- parse_env:

  environment used to parse module

## Value

A list of server utility functions; see 'Examples' below.

## Examples

``` r
# 'RAVE' module server function
server <- function(input, output, session, ...) {
  shidashi::init_app()

  pipeline_path <- "PATH to module pipeline"

  module_server_common(
    module_id = session$ns(NULL),
    check_data_loaded = function(first_time) {

      re <- tryCatch({
        # Try to read data from pipeline results
        repo <- ravepipeline::pipeline_read(
          'repository',
          pipe_dir = pipeline_path
        )

        # Fire event to update footer message
        ravedash::fire_rave_event('loader_message',
                                  "Data loaded")

        # Return TRUE indicating data has been loaded
        TRUE
      }, error = function(e) {

        # Fire event to remove footer message
        ravedash::fire_rave_event('loader_message', NULL)

        # Return FALSE indicating no data has been found
        FALSE
      })
    }, session = session
  )

}
```
