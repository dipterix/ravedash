# Button to trigger data loader

A button that triggers `'load_data'` event, which runs scripts
registered via `set_script` with `binding_event = "load_data"` (see
[`module_server_common`](https://dipterix.org/ravedash/reference/module_server_common.md));
the same script can be triggered programmatically via `trigger_script`.

## Usage

``` r
load_data_button(
  label = "Load data",
  icon = NULL,
  width = NULL,
  type = "primary",
  btn_type = c("button", "link"),
  class = "",
  style = "",
  ...
)
```

## Arguments

- label:

  label to display

- icon:

  icon before the label

- width, class, style, ...:

  passed to 'HTML' tag

- type:

  used to calculate `class`

- btn_type:

  button style, choices are `'button'` or `'link'`

## Value

A 'HTML' button tag

## See also

[`run_analysis_button`](https://dipterix.org/ravedash/reference/run_analysis_button.md),
[`get_rave_event`](https://dipterix.org/ravedash/reference/rave-runtime-events.md)

## Examples

``` r

# In loader UI
load_data_button("Load subject", width = "100%")
#> <button class="btn btn-primary rave-button" style="width: 100%;" type="button" rave-action="{&quot;type&quot;: &quot;load_data&quot;}">
#>   
#>   Load subject
#> </button>

# In loader server function
if (FALSE) { # \dontrun{
server_tools <- get_default_handlers(session = session)
server_tools$set_script(
  "load_data",
  {
    # load data ...
  },
  binding_event = "load_data",
  dispatch_event = "data_changed",
  alert_params = list(title = "Loading in progress")
)
} # }
```
