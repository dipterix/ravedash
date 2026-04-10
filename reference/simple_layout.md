# Simple input-output layout

Provides simple layout, with inputs on the left, and outputs on the
right. Only useful in 'shidashi' framework.

## Usage

``` r
simple_layout(
  input_ui,
  output_ui,
  input_width = 4L,
  container_fixed = FALSE,
  container_style = NULL,
  scroll = FALSE
)
```

## Arguments

- input_ui:

  the 'HTML' tags for the inputs

- output_ui:

  the 'HTML' tags for the outputs

- input_width:

  width of inputs, must be an integer from 1 to 11

- container_fixed:

  whether the maximum width of the container should be fixed; default is
  no

- container_style:

  additional 'CSS' style of the container

- scroll:

  whether to stretch the container to full-heights and scroll the input
  and output separately.

## Value

'HTML' tags

## Examples

``` r
library(shiny)
library(ravedash)

simple_layout(
  input_ui = list(
    ravedash::input_card(
      title = "Data Selection",
      "Add inputs here"
    )
  ),
  output_ui = list(
    ravedash::output_card(
      title = "Result A",
      "Add outputs here"
    )
  )
)
#> <div class="container-fluid">
#>   <div class="row">
#>     <div class="col-md-4">
#>       <div class="row">
#>         <div class="col-sm-12"><div class="card card-single ravedash-input-card " data-title="Data Selection">
#>   <div class="card-header shidashi-anchor" style="">
#>     <h5 class="card-title">Data Selection</h5>
#>     <div class="card-tools">
#>             <a type="button" href="https://openwetware.org/wiki/RAVE:&lt;placeholder for module ID&gt;:input_dataselection" target="_blank" class="btn btn-tool" data-card-widget="link">
#>               <i class="far fa-circle-question fas" role="presentation" aria-label="circle-question icon" verify_fa="FALSE"></i>
#>             </a>
#>             <a type="button" href="#" target="_self" class="btn btn-tool" data-card-widget="collapse">
#>               <i class="fas fa-minus" role="presentation" aria-label="minus icon"></i>
#>             </a>
#>           </div>
#>   </div>
#>   <div class="card-body fill-width fill-height padding-10" style=";">
#>     Add inputs here
#>   </div>
#>   
#> </div>
#> </div>
#>       </div>
#>     </div>
#>     <div class="col-md-8">
#>       <div class="row">
#>         <div class="col-sm-12"><div class="card card-single ravedash-output-card " data-title="Result A">
#>   <div class="card-header " style="">
#>     <h5 class="card-title">Result A</h5>
#>     <div class="card-tools">
#>             <a type="button" href="https://openwetware.org/wiki/RAVE:&lt;placeholder for module ID&gt;:output_resulta" target="_blank" class="btn btn-tool" data-card-widget="link">
#>               <i class="far fa-circle-question fas" role="presentation" aria-label="circle-question icon" verify_fa="FALSE"></i>
#>             </a>
#>             <a type="button" href="#" target="_self" class="btn btn-tool" data-card-widget="collapse">
#>               <i class="fas fa-minus" role="presentation" aria-label="minus icon"></i>
#>             </a>
#>             <a type="button" href="#" target="_self" class="btn btn-tool" data-card-widget="maximize">
#>               <i class="fas fa-expand" role="presentation" aria-label="expand icon"></i>
#>             </a>
#>           </div>
#>   </div>
#>   <div class="card-body fill-width fill-height padding-10" style=";">
#>     Add outputs here
#>   </div>
#>   
#> </div>
#> </div>
#>       </div>
#>     </div>
#>   </div>
#> </div>

```
