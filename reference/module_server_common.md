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

  shiny session

- parse_env:

  environment used to parse module

## Value

A list of server utility functions; see 'Examples' below.

## Examples

``` r
# Debug in non-reactive session: create fake session
fake_session <- shiny::MockShinySession$new()

# register common-server function
module_server_common(module_id = "mock-session",
                     session = fake_session)
server_tools <- get_default_handlers(fake_session)

# Print each function to see the usage

server_tools$auto_recalculate
#> Function to turn auto-recalculation on and off. Usage:
#> 
#> # Obtain the server utility functions
#> server_tools <- get_default_handlers()
#> 
#> server_tools$auto_recalculate(TRUE)    # Turn on forever
#> server_tools$auto_recalculate(FALSE)   # Turn off forever
#> server_tools$auto_recalculate(1)       # Turn on once

server_tools$run_analysis_onchange
#> Function to set input IDs to watch. These inputs will trigger auto re-calculate. Usage:
#> 
#> # Obtain the server utility functions
#> server_tools <- get_default_handlers()
#> 
#> server_tools$run_analysis_onchange(c("inputId_1", "inputId_2", ...))

server_tools$run_analysis_flag
#> Flag to run analysis pipeline. Usage:
#> 
#> # Obtain the server utility functions
#> server_tools <- get_default_handlers()
#> 
#> shiny::bindEvent(
#>   shiny::observe({
#>     <Run your algorithms, e.g. `ravepipeline::pipeline_run(...)>
#>   }),
#>   server_tools$run_analysis_flag(),
#>   ignoreNULL = TRUE, ignoreInit = TRUE
#> )

server_tools$module_is_active
#> Shiny reactive to tell if the module is active or hidden. Usage:
#> 
#> # Obtain the server utility functions
#> server_tools <- get_default_handlers()
#> 
#> server_tools$module_is_active()                   # current module
#> server_tools$module_is_active("another_module")   # whether another module is active

server_tools$simplify_view
#> Show, hide, or toggle display of elements with `rave-optional` HTML class. Usage:
#> 
#> # Obtain the server utility functions
#> server_tools <- get_default_handlers()
#> 
#> server_tools$simplify_view()       # Toggle optional view
#> server_tools$simplify_view('yes')  # Show optional view
#> server_tools$simplify_view('no')   # Hide optional view

# 'RAVE' module server function
server <- function(input, output, session, ...){

  pipeline_path <- "PATH to module pipeline"

  module_server_common(
    module_id = session$ns(NULL),
    check_data_loaded = function(first_time){

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
      }, error = function(e){

        # Fire event to remove footer message
        ravedash::fire_rave_event('loader_message', NULL)

        # Return FALSE indicating no data has been found
        FALSE
      })
    }, session = session
  )

}
```
