# Safe-wrapper of 'shiny' [`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html) function

A reactive expression whose errors never reach its consumers: an error
is logged (and optionally shown) and the expression yields the last
value it produced, or `initial` before its first success. Validation
errors ([`req`](https://rdrr.io/pkg/shiny/man/req.html),
[`validate`](https://rdrr.io/pkg/shiny/man/validate.html)) pass through,
so outputs still stop rendering as they do with
[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html).

## Usage

``` r
safe_reactive(
  x,
  env = parent.frame(),
  quoted = FALSE,
  ...,
  label = NULL,
  domain = shiny::getDefaultReactiveDomain(),
  initial = NULL,
  error_wrapper = c("none", "notification", "alert")
)
```

## Arguments

- x, env, quoted, label, domain, ...:

  passed to [`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)

- initial:

  value returned while no evaluation has succeeded yet

- error_wrapper:

  handler when error is encountered, choices are `'none'` (log only),
  `'notification'` (see
  [`error_notification`](https://dipterix.org/ravedash/reference/with_error_notification.md)),
  or `'alert'` (see
  [`error_alert`](https://dipterix.org/ravedash/reference/with_error_notification.md))

## Value

'shiny' reactive expression

## Details

'shiny' caches the error raised by a reactive expression and raises it
again to every consumer until the expression is invalidated; when the
consumer is an observer, or the expression is a trigger of
[`bindEvent`](https://rdrr.io/pkg/shiny/man/bindEvent.html), the session
is closed. The last good value therefore lives in this wrapper, not in
the reactive's cache.

## Examples

``` r

values <- shiny::reactiveValues(A = 1)

doubled <- safe_reactive({
  if (values$A < 0) { stop("A must be non-negative") }
  values$A * 2
})

# In a running app, `doubled()` keeps its last value when `values$A` turns
# negative; the error goes to the log instead of closing the session
```
