# Button to trigger analysis

A button that triggers `'run_analysis'` event; see also
[`get_rave_event`](https://dipterix.org/ravedash/reference/rave-runtime-events.md)

## Usage

``` r
run_analysis_button(
  label = "Run analysis (Ctrl+Enter)",
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
