# A hovering footer at bottom-right

Internally used. Do not call explicitly

## Usage

``` r
ravedash_footer(
  module_id = NULL,
  label = "Run Analysis",
  auto_recalculation = TRUE,
  message_action = "toggle_loader",
  class = NULL,
  style = NULL
)
```

## Arguments

- module_id:

  'RAVE' module ID

- label:

  run-analysis button label; default is `"Run Analysis"`

- auto_recalculation:

  whether to show the automatic calculation button; default is true

- message_action:

  message to send when clicking on message button; default is
  `'toggle_loader'`, which opens up loading screen

- class:

  additional class for the footer

- style:

  additional style for the footer

## Value

'HTML' tags

## Examples

``` r
library(shiny)
# dummy variables for the example
data_loaded <- TRUE

# UI code
ravedash_footer("my_module")
#> <div class="ravedash-back-to-top ravedash-footer">
#>   <div class="btn-group dropup" role="group">
#>     <button type="button" id="my_module-__loader_short_message__" class="btn btn-default border-right-1 btn-go-top shiny-text-output rave-button" data-toggle="tooltip" title="Click to toggle the data loader" rave-action="{&quot;type&quot;: &quot;toggle_loader&quot;}"></button>
#>     <button type="button" class="btn btn-default border-right-1 border-left-1 rave-button rave-button-autorecalculate" data-toggle="tooltip" title="Keyboard shortcut: CTRL+Enter / Command+Return (OSX)" rave-action="{&quot;type&quot;: &quot;run_analysis&quot;}">Run Analysis</button>
#>     <button type="button" class="btn btn-default btn-go-top border-left-1 dropdown-toggle dropdown-toggle-split" href="#" aria-haspopup="false" aria-expanded="false">
#>       <span class="sr-only">Dropdown-Open</span>
#>     </button>
#>     <div class="dropdown-menu dropdown-menu-right">
#>       <h6 class="dropdown-header">Controllers</h6>
#>       <div class="px-3 py-1">
#>         <a class="btn btn-default rave-button" href="#" rave-action="{&quot;type&quot;: &quot;toggle_auto_recalculation&quot;}" data-toggle="tooltip" title="Toggle auto re-calculation">
#>           <i class="fas fa-arrows-rotate " role="presentation" aria-label="arrows-rotate icon" verify_fa="FALSE"></i>
#>           <span style="color: #007bff" id="my_module-__recalculation_message__" class="shiny-text-output pointer-events-none"></span>
#>         </a>
#>         <a class="btn btn-default shidashi-button" href="#" shidashi-action="{&quot;method&quot;: &quot;card&quot;, &quot;args&quot;: [{&quot;selector&quot;: &quot;.ravedash-input-card&quot;, &quot;method&quot;: &quot;expand&quot;}]}" data-toggle="tooltip" title="Expand all input cards">
#>           <i class="fas fa-plus " role="presentation" aria-label="plus icon" verify_fa="FALSE"></i>
#>         </a>
#>         <a class="btn btn-default shidashi-button" href="#" shidashi-action="{&quot;method&quot;: &quot;card&quot;, &quot;args&quot;: [{&quot;selector&quot;: &quot;.ravedash-input-card&quot;, &quot;method&quot;: &quot;collapse&quot;}]}" data-toggle="tooltip" title="Collapse all input cards">
#>           <i class="fas fa-minus " role="presentation" aria-label="minus icon" verify_fa="FALSE"></i>
#>         </a>
#>         <a class="btn btn-default rave-button" href="#" rave-action="{&quot;type&quot;: &quot;simplify_toggle&quot;}" data-toggle="tooltip" title="Show more/fewer options">
#>           <i class="fab fa-simplybuilt " role="presentation" aria-label="simplybuilt icon" verify_fa="FALSE"></i>
#>         </a>
#>       </div>
#>       <div class="dropdown-divider"></div>
#>       <h6 class="dropdown-header">Quick Access</h6>
#>     </div>
#>   </div>
#> </div>

# server code to set message
server <- function(input, output, session) {

  module_server_common(input, output, session, function() {

    # check if data has been loaded
    if (data_loaded) {

      # if yes, then set the footer message
      fire_rave_event("loader_message",
                      "my_project/subject - Epoch: Auditory")
      return(TRUE)
    } else {

      # No data found, unset the footer message
      fire_rave_event("loader_message", NULL)
      return(FALSE)
    }

  })
}
```
