# Input and output card (front-end element)

Input and output card (front-end element)

## Usage

``` r
input_card(
  title,
  ...,
  class = "",
  class_header = "shidashi-anchor",
  class_body = "padding-10",
  class_foot = "padding-10",
  href = "auto",
  tools = NULL,
  footer = NULL,
  append_tools = TRUE,
  toggle_advanced = FALSE,
  module_id = get0("module_id", ifnotfound = NULL, envir = parent.frame())
)

output_card(
  title,
  ...,
  class = "",
  class_body = "padding-10",
  class_foot = "padding-10",
  href = "auto",
  tools = NULL,
  append_tools = TRUE,
  module_id = get0("module_id", ifnotfound = NULL, envir = parent.frame())
)

output_cardset(
  title,
  ...,
  class = "",
  class_body = "no-padding",
  class_foot = "padding-10",
  href = "auto",
  tools = NULL,
  append_tools = TRUE,
  module_id = get0("module_id", ifnotfound = NULL, envir = parent.frame())
)
```

## Arguments

- title:

  title of the card

- ...:

  additional elements to be included in the card, see
  [`card`](https://dipterix.org/shidashi/reference/card.html)

- class:

  the 'HTML' class for card

- class_header:

  the 'HTML' class for card header; default is `'shidashi-anchor'`,
  which will generate shortcuts at the page footers

- class_body:

  the 'HTML' class for card body; default is `"padding-10"`, with
  `'10px'` at each direction

- class_foot:

  the 'HTML' class for card footer; default is `"padding-10"`, with
  `'10px'` at each direction

- href:

  hyper reference link of the card

- tools:

  a list of additional card tools, see
  [`card_tool`](https://dipterix.org/shidashi/reference/card_tool.html)

- footer:

  footer elements

- append_tools:

  whether to append `tools` to the default list; default is true

- toggle_advanced:

  whether to show links in the footer to toggle elements with 'HTML'
  class `'rave-optional'`

- module_id:

  the 'RAVE' module ID

## Value

'HTML' tags

## See also

[`card`](https://dipterix.org/shidashi/reference/card.html)

## Examples

``` r

input_card(title = "Condition selector",
           "Please select experimental conditions:",
           shiny::selectInput(
             inputId = "condition", label = "Condition",
             choices = c("Audio", "Visual")
           ))
#> <div class="card card-single ravedash-input-card " data-title="Condition selector">
#>   <div class="card-header shidashi-anchor" style="">
#>     <h5 class="card-title">Condition selector</h5>
#>     <div class="card-tools">
#>   <a type="button" href="https://openwetware.org/wiki/RAVE:&lt;placeholder for module ID&gt;:input_conditionselector" target="_blank" class="btn btn-tool" data-card-widget="link">
#>     <i class="far fa-circle-question fas" role="presentation" aria-label="circle-question icon" verify_fa="FALSE"></i>
#>   </a>
#>   <a type="button" href="#" target="_self" class="btn btn-tool" data-card-widget="collapse">
#>     <i class="fas fa-minus" role="presentation" aria-label="minus icon"></i>
#>   </a>
#> </div>
#>   </div>
#>   <div class="card-body fill-width fill-height padding-10" style=";">
#>     Please select experimental conditions:
#> <div class="form-group shiny-input-container">
#>   <label class="control-label" id="condition-label" for="condition">Condition</label>
#>   <div>
#>     <select id="condition" class="shiny-input-select"><option value="Audio" selected>Audio</option>
#> <option value="Visual">Visual</option></select>
#>     <script type="application/json" data-for="condition" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#>   </div>
#> </div>
#>   </div>
#>   
#> </div>
#> 
```
