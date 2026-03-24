# Logger system used by 'RAVE'

Keep track of messages printed by modules

## Usage

``` r
logger(
  ...,
  level = c("info", "warning", "error", "fatal", "debug", "trace"),
  calc_delta = "auto",
  .envir = parent.frame(),
  .sep = "",
  use_glue = FALSE,
  reset_timer = FALSE
)

set_logger_path(root_path, max_bytes, max_files)

logger_threshold(
  level = c("info", "warning", "error", "fatal", "debug", "trace"),
  module_id,
  type = c("console", "file", "both")
)

logger_error_condition(cond, level = "error")

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

- ..., .envir, .sep:

  passed to `glue`, if `use_glue` is true

- level:

  the level of message, choices are `'info'` (default), `'warning'`,
  `'error'`, `'fatal'`, `'debug'`, `'trace'`

- calc_delta:

  whether to calculate time difference between current message and
  previous message; default is `'auto'`, which prints time difference
  when `level` is `'debug'`. This behavior can be changed by altering
  `calc_delta` by a logical `TRUE` to enable or `FALSE` to disable.

- use_glue:

  whether to use `glue` to combine `...`; default is false

- reset_timer:

  whether to reset timer used by `calc_delta`

- root_path:

  root directory if you want log messages to be saved to hard disks; if
  `root_path` is `NULL`, `""`, or
  [`nullfile`](https://rdrr.io/r/base/showConnections.html), then logger
  path will be unset.

- max_bytes:

  maximum file size for each logger partitions

- max_files:

  maximum number of partition files to hold the log; old files will be
  deleted.

- module_id:

  'RAVE' module identification string, or name-space; default is
  `'ravedash'`

- type:

  which type of logging should be set; default is `'console'`, if file
  log is enabled through `set_logger_path`, `type` could be `'file'` or
  `'both'`. Default log level is `'info'` on console and `'debug'` on
  file.

- cond:

  condition to log

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

## Value

The message without time-stamps

## Examples

``` r
logger("This is a message")

a <- 1
logger("A message with glue: a={a}")

logger("A message without glue: a={a}", use_glue = FALSE)


logger("Message A", calc_delta = TRUE, reset_timer = TRUE)
logger("Seconds before logging another message", calc_delta = TRUE)


# by default, debug and trace messages won't be displayed
logger('debug message', level = 'debug')

# adjust logger level, make sure `module_id` is a valid RAVE module ID
logger_threshold('debug', module_id = NULL)

# Debug message will display
logger('debug message', level = 'debug')

# Trace message will not display as it's lower than debug level
logger('trace message', level = 'trace')
```
