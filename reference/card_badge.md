# Create a badge widget located at card header

Create a badge widget located at card header

## Usage

``` r
card_badge(text = NULL, class = NULL, ...)

card_recalculate_badge(text = "Recalculate needed", class = NULL, ...)

enable_recalculate_badge(text = "Recalculate needed", ...)

disable_recalculate_badge(text = "Up-to-date", ...)

set_card_badge(
  id = NULL,
  class = NULL,
  text = NULL,
  add_class = NULL,
  remove_class = NULL,
  session = shiny::getDefaultReactiveDomain()
)
```

## Arguments

- text:

  inner text content of the badge

- class:

  additional 'HTML' class of the badge; for `set_card_badge`, this is
  the class selector of the badge that is to be changed

- ...:

  additional 'HTML' tag attributes

- id:

  the badge 'HTML' ID to be changed, will be enclosed with session
  namespace `session$ns(id)` automatically.

- add_class, remove_class:

  add or remove class

- session:

  shiny session

## Examples

``` r
library(ravedash)

# UI: a Bootstrap badge with green background
card_badge("Ready", class = "bg-green rave-output-status")
#> <span class="right badge rave-card-badge bg-green rave-output-status">Ready</span>

# server
server <- function(input, output, session) {

  safe_observe({

    # ... check if the inputs have changed

    set_card_badge(
      class = "rave-output-status",
      text = "Refresh needed",
      add_class = "bg-yellow",
      remove_class = "bg-green"
    )

  })

}
```
