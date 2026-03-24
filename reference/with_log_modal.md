# Evaluate script in the background and show the results from shiny modal dialogue

Evaluate script in the background and show the results from shiny modal
dialogue

## Usage

``` r
with_log_modal(
  expr,
  quoted = FALSE,
  callback = NULL,
  title = "Running...",
  size = "l",
  session = shiny::getDefaultReactiveDomain(),
  ...
)
```

## Arguments

- expr:

  R expression to evaluate The script must be standalone

- quoted:

  whether the expression has been quoted

- callback:

  callback function to run once the evaluate finishes; must take one
  argument. The passed variable will be the evaluation results or an
  error condition (if error occurs)

- title, size:

  modal title and size, see
  [`showModal`](https://rdrr.io/pkg/shiny/man/showModal.html)

- session:

  shiny session object

- ...:

  ignored, reserved for future use

## Value

A promise object

## Examples

``` r
# Shiny server function
server <- function(input, output, session) {
  shiny::bindEvent(
    shiny::observe({
      with_log_modal(
        title = "Roll the dice",
        expr = {
          for(i in 1:10) {
            Sys.sleep(runif(1, min = 0.5, max = 2))
            cat(sprintf("Rolling dice result: %.0f\n", sample(6, 1)))
          }
        }
      )
      return()
    }),
    input$btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
}

if(interactive()) {
  shiny::shinyApp(
    ui = shiny::basicPage(
      shiny::actionButton('btn', "Click me")
    ),
    server = server,
    options = list(launch.browser = TRUE)
  )
}

```
