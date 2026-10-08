# Changelog

## ravedash 0.1.4

- Preset inputs carry hints for `MCP` agents (see
  `shidashi::input_hint_classes`): loader inputs such as the project,
  subject, epoch, trial window, reference, and electrodes are
  `loader_mandatory` (agents ask the user before loading data), analysis
  inputs such as condition groups, baseline windows, and analysis ranges
  are `analysis_mandatory`, and the inputs that create subjects are
  `loader_forbidden`. Presets gain a `hint` argument to override them,
  by input ID or for all inputs of a preset; requires `shidashi`
  0.2.0.17
- Module servers report to agents whether the data loader is open
  (`@state$loader_opened` in `shidashi`’s `shiny_input_info`), so agents
  do not ask loader questions once the data are loaded

## ravedash 0.1.3

- Added
  [`safe_reactive()`](https://dipterix.org/ravedash/reference/safe_reactive.md):
  a reactive expression that logs its errors and keeps returning its
  last good value, so a failing reactive cannot close the session
  through an observer or a
  [`bindEvent()`](https://rdrr.io/pkg/shiny/man/bindEvent.html) trigger;
  validation errors ([`req()`](https://rdrr.io/pkg/shiny/man/req.html),
  [`validate()`](https://rdrr.io/pkg/shiny/man/validate.html)) still
  pass through
- Preset inputs, the import-block status and preview outputs, and the
  report wizard are registered with `shidashi` so that `MCP` agents can
  query and update them; presets and
  [`create_report_wizard()`](https://dipterix.org/ravedash/reference/create_report_wizard.md)
  gain an `env` argument (the module environment,
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) by default)
  to find the module’s registry. Inputs that create subjects or projects
  are registered as read-only; requires `shidashi` 0.2.0
- Added
  [`load_data_button()`](https://dipterix.org/ravedash/reference/load_data_button.md);
  module servers can register named scripts via `set_script()`
  (optionally bound to a ‘RAVE’ event, with an alert and a follow-up
  event) and run them programmatically via `trigger_script()` (see
  [`get_default_handlers()`](https://dipterix.org/ravedash/reference/rave-runtime-events.md)),
  for example, from `MCP` tools
- Allow preset-components to access to pipeline instance
- Added `strip_style` to remove `ansi` styles
- Changed `logger_error_condition` to `S3` generics
- Added `error_alert` similar to `error_notification`, but using
  [`dipsaus::shiny_alert2`](https://dipterix.org/dipsaus/reference/shiny_alert2.html)
- Fixed a preset that cannot fails correctly with validation errors when
  importing channels
- `safe_wrap_expr` wraps error handlers
- Reexported some `dipsaus` and `shidashi` functions so users do not
  have to import them
- `save_observe` has better error handling method
- Raising `rave_muffled` errors in `save_observe` will not trigger
  notifications nor error alerts, but will print out error details

## ravedash 0.1.2

CRAN release: 2022-10-15

- Added presets to support importing `.nev` files with the most recent
  `raveio` updates
- Added shutdown function to allow users to shutdown instances from the
  interface, and added single-session mode to automatically shutdown the
  server once the session window is closed
- Fixed loader brain not updating issue when electrode table is empty
- Allowed run-analysis button to be placed on bottom-left with
  additional styles
- Added `error_notification` and `with_error_notification` to easily
  display error messages to the dashboard
- Allows modules to create session-based temporary files and options
  (experimental)
- Shorten response time needed to fire analysis event
- Moved `htmltools` to suggests
- Included free version of `fontawesome` into the build to avoid further
  changes
- Replaced `pickerInput` with native `selectInput` in condition builder
- Added `start_session` to start a new/existing session with one
  function
- Added stand-alone viewer support, allowing almost any outputs to be
  displayed in another session in full-screen, and synchronized with the
  main session
- Added `register_output` to register outputs, and `get_output_options`
  to obtain render details. This replaces the shiny output assign and
  facilitates the stand-alone viewer
- Added `output_gadget` and `output_gadget_container` to display
  built-in gadgets to outputs, allowing users to download outputs, and
  to display them in stand-alone viewers; needs little extra setups,
  must be used with `register_output`
- Added session-level (application) temporary directory and files (see
  `temp_dir` and `temp_file`), based on the application-level storage,
  created `session_setopt` and `session_getopt`

## ravedash 0.1.1

CRAN release: 2022-06-23

- Added a `NEWS.md` file to track changes to the package.
