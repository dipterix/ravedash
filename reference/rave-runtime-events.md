# 'RAVE' run-time events

A set of preset behaviors used by 'RAVE' modules

## Usage

``` r
register_rave_session(
  session = shiny::getDefaultReactiveDomain(),
  .rave_id = NULL
)

get_default_handlers(session = shiny::getDefaultReactiveDomain())

fire_rave_event(
  key,
  value,
  global = FALSE,
  force = FALSE,
  session = shiny::getDefaultReactiveDomain(),
  .internal_ok = FALSE
)

get_rave_event(key, session = shiny::getDefaultReactiveDomain())

open_loader(session = shiny::getDefaultReactiveDomain())

close_loader(session = shiny::getDefaultReactiveDomain())

watch_loader_opened(session = shiny::getDefaultReactiveDomain())

watch_data_loaded(session = shiny::getDefaultReactiveDomain())

current_shiny_theme(default, session = shiny::getDefaultReactiveDomain())
```

## Arguments

- session:

  shiny session, usually automatically determined

- key:

  event key to fire or to monitor

- value:

  event value

- global:

  whether to notify other sessions (experimental and not recommended)

- force:

  whether to force firing the event even the `value` hasn't changed

- .internal_ok:

  internally used

- default:

  default value if not found

- rave_id, .rave_id:

  deprecated, no longer used

## Value

See 'Details'

## Details

These goal of these event functions is to simplify the dashboard logic
without understanding the details or passing global variables around.
Everything starts with
[`shidashi::register_session`](https://dipterix.org/shidashi/reference/register_session.html).
This function registers session event handlers and theme observers. If
you have called
[`module_server_common`](https://dipterix.org/ravedash/reference/module_server_common.md),
then session registration has already been done.

- `register_rave_session`:

  **Deprecated.** Use
  [`shidashi::register_session()`](https://dipterix.org/shidashi/reference/register_session.html)
  instead.

- `fire_rave_event`:

  send signals to make changes to a event; returns nothing

- `get_rave_event`:

  watch and get the event values; must run in shiny reactive context

- `open_loader`:

  fire an event with a special key `'open_loader'` to open the
  data-loading panel; returns nothing

- `close_loader`:

  reset an event with a special key `'open_loader'` to close the
  data-loading panel if possible; returns nothing

- `watch_loader_opened`:

  watch in shiny reactive context whether the loader is opened; returns
  a logical value, but raise errors when reactive context is missing

- `watch_data_loaded`:

  watch a special event with key `'data_loaded'`; returns a logical
  value of whether new data has been loaded, or raise errors when
  reactive context is missing

- `current_shiny_theme`:

  watch and returns a list of theme parameters, for example, light or
  dark theme

## Built-in Events

The following event keys are built-in. Please do not fire them using
`fire_rave_event` or the 'RAVE' application might will crash

- `'simplify_toggle'`:

  toggle visibility of 'HTML' elements with class `'rave-option'`

- `'run_analysis'`:

  notifies the module to run pipeline

- `'save_pipeline'`, `'load_pipeline'`:

  notifies the module to save or load pipeline

- `'data_loaded'`:

  notifies the module that new data has been loaded

- `'open_loader'`, `'toggle_loader'`:

  notifies the internal server code to show or hide the data loading
  panel

- `'active_module'`:

  internally populated by the action dispatcher when the module first
  loads (and whenever a `type = "active_module"` rave-action arrives).
  Returns a list with elements `type`, `id`, `parent_frame`, and
  `rave_id`. This key is **reserved**: external code must not call
  `fire_rave_event("active_module", ...)` directly. To obtain the
  currently active module, prefer
  [`get_active_module_info()`](https://dipterix.org/ravedash/reference/get_active_module_info.md),
  which reads from the `shidashi` module registry and is always
  up-to-date.

## Examples

``` r

library(shiny)
library(ravedash)

ui <- fluidPage(
  actionButton("btn", "Fire event"),
  actionButton("btn2", "Toggle loader")
)

server <- function(input, output, session) {
  # Create event registries
  shidashi::register_session()

  shiny::bindEvent(
    shiny::observe({
      fire_rave_event("my_event_key", Sys.time())
    }),
    input$btn,
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )
  shiny::bindEvent(
    shiny::observe({
      cat("An event fired with value:", get_rave_event("my_event_key"), "\n")
    }),
    get_rave_event("my_event_key"),
    ignoreNULL = TRUE
  )

  shiny::bindEvent(
    shiny::observe({
      if (watch_loader_opened()) {
        close_loader()
      } else {
        open_loader()
      }
    }),
    input$btn2,
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  shiny::bindEvent(
    shiny::observe({
      cat("Loader ", ifelse(watch_loader_opened(), "opened", "closed"), "\n")
    }),
    watch_loader_opened(),
    ignoreNULL = TRUE
  )

}

if(interactive()) {
  shinyApp(ui, server)
}
```
