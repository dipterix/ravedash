# Group input elements into a box with title

Only works in template framework provided by `'shidashi'` package, see
[`use_template`](https://dipterix.org/shidashi/reference/use_template.html)

## Usage

``` r
group_box(title, ..., class = NULL)

flex_group_box(title, ..., class = NULL, wrap = "wrap", direction = "row")
```

## Arguments

- title:

  the box title

- ...:

  elements to be included or to be passed to other methods

- class:

  additional class of the box

- wrap, direction:

  see
  [`flex_container`](https://dipterix.org/shidashi/reference/flex_container.html)

## Value

A 'HTML' tag

## Examples

``` r

library(shiny)
library(shidashi)
library(ravedash)

group_box(
  title = "Analysis Group A",
  selectInput("a", "Condition", choices = c("A", "B")),
  sliderInput("b", "Time range", min = 0, max = 1, value = c(0,1))
)
#> <div class="group-input-box" title="Analysis Group A">
#>   <div class="form-group shiny-input-container">
#>     <label class="control-label" id="a-label" for="a">Condition</label>
#>     <div>
#>       <select id="a" class="shiny-input-select"><option value="A" selected>A</option>
#> <option value="B">B</option></select>
#>       <script type="application/json" data-for="a" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#>     </div>
#>   </div>
#>   <div class="form-group shiny-input-container">
#>     <label class="control-label" id="b-label" for="b">Time range</label>
#>     <input class="js-range-slider" id="b" data-skin="shiny" data-type="double" data-min="0" data-max="1" data-from="0" data-to="1" data-step="0.01" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-drag-interval="true" data-data-type="number"/>
#>   </div>
#> </div>

flex_group_box(
  title = "Project and Subject",
  flex_item( "Some input 1" ),
  flex_item( "Some input 2" ),
  flex_break(),
  flex_item( "Some input in new line" )
)
#> <div style="flex-direction:row; flex-wrap:wrap; justify-content:flex-start; align-content:stretch; align-items:stretch; display:flex" class="group-input-box" title="Project and Subject">
#>   <div style="flex:1" class="fill-width padding-5">Some input 1</div>
#>   <div style="flex:1" class="fill-width padding-5">Some input 2</div>
#>   <div class="flex-break"></div>
#>   <div style="flex:1" class="fill-width padding-5">Some input in new line</div>
#> </div>
```
