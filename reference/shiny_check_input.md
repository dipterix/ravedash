# Check shiny inputs and modify if validation fails

Check shiny inputs and modify if validation fails

## Usage

``` r
shiny_check_input(
  inputId,
  check = NULL,
  on_check_fails,
  ...,
  quoted = FALSE,
  env = parent.frame(),
  logger_level = c("trace", "none", "debug", "info", "warning", "error"),
  session = shiny::getDefaultReactiveDomain()
)
```

## Arguments

- inputId:

  character, input ID

- check:

  either a function that takes the input value or a character of a
  `checkmate` function; when `check` is a character, this function will
  look for `check_*` functions in the `checkmate` package

- on_check_fails:

  value to substitute when check fails, and the input value will be the
  result of `on_check_fails`. This argument can be missing; when
  missing, input value will not be altered

- ...:

  passed to `check` function

- quoted:

  whether `on_check_fails` is quoted

- env:

  environment to evaluate `on_check_fails`

- logger_level:

  log level when validation fails

- session:

  shiny session; default is current session

## Value

A shiny observe instance

## Examples

``` r
if(interactive()) {

library(ravedash)
shiny::shinyApp(
  ui = shiny::basicPage(
    shiny::textInput("id1", "Enter a text"),
    shiny::textOutput("id2")
  ),
  server = function(input, output, session) {
    # input$id1 must have at least 1 character
    # the check uses `checkmate::check_character`
    shiny_check_input(
      "id1",
      check = "character",
      min.chars = 1,
      on_check_fails = "altered text"
    )

    output$id2 <- shiny::renderText({
      print(input$id1)
      sprintf("The final value is: %s", input$id1)
    })
  }
)

}
```
