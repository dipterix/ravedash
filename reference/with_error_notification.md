# Evaluate with automatic error handlers in dashboard

Keep track of messages printed by modules

## Usage

``` r
error_notification(
  cond,
  title = "Error found!",
  type = "danger",
  class = "error_notif",
  delay = 30000,
  autohide = TRUE,
  collapse = "\n",
  prefix = paste("Found the following error",
    "(details have been printed in the console):"),
  session = shiny::getDefaultReactiveDomain()
)

error_alert(
  cond,
  title = "Error found!",
  type = "error",
  danger_mode = TRUE,
  auto_close = FALSE,
  prefix = paste("Found the following error",
    "(details have been printed in the console):"),
  buttons = "Confirm",
  session = shiny::getDefaultReactiveDomain()
)

with_error_notification(expr, envir = parent.frame(), quoted = FALSE, ...)

with_error_alert(expr, envir = parent.frame(), quoted = FALSE, ...)
```

## Arguments

- cond:

  condition to log

- type:

  which type of logging should be set; default is `'console'`, if file
  log is enabled through `set_logger_path`, `type` could be `'file'` or
  `'both'`. Default log level is `'info'` on console and `'debug'` on
  file.

- class, title, delay, autohide:

  passed to
  [`show_notification`](https://dipterix.org/shidashi/reference/notification.html)

- collapse, danger_mode, auto_close, buttons:

  will be passed to
  [`shiny_alert2`](https://dipterix.org/dipsaus/reference/shiny_alert2.html)
  or
  [`show_notification`](https://dipterix.org/shidashi/reference/notification.html)

- prefix:

  additional messages to display in the notification or alert

- session:

  shiny session

- expr:

  expression to evaluate

- envir:

  environment to evaluate `expr`

- quoted:

  whether `expr` is quoted; default is false

- ...:

  passed to other methods

## Value

The condition `cond` if errored out, or the evaluated results
