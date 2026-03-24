# Safe-wrapper of 'shiny' [`observe`](https://rdrr.io/pkg/shiny/man/observe.html) function

Safely wrap expression `x` such that shiny application does no hang when
when the expression raises error.

## Usage

``` r
safe_observe(
  x,
  env = NULL,
  quoted = FALSE,
  priority = 0L,
  domain = NULL,
  ...,
  error_wrapper = c("none", "notification", "alert"),
  watch_data = getOption("ravedash.auto_watch_data", FALSE)
)
```

## Arguments

- x, env, quoted, priority, domain, ...:

  passed to [`observe`](https://rdrr.io/pkg/shiny/man/observe.html)

- error_wrapper:

  handler when error is encountered, choices are `'none'`,
  `'notification'` (see
  [`error_notification`](https://dipterix.org/ravedash/reference/logger.md)),
  or `'alert'` (see
  [`error_alert`](https://dipterix.org/ravedash/reference/logger.md))

- watch_data:

  whether to invalidate only when
  [`watch_data_loaded`](https://dipterix.org/ravedash/reference/rave-runtime-events.md)
  is `TRUE`

## Value

'shiny' observer instance

## Examples

``` r
values <- shiny::reactiveValues(A=1)

obsB <- safe_observe({
  print(values$A + 1)
})
```
