# Get current active module information, internally used

Get current active module information, internally used

## Usage

``` r
get_active_module_info(session = shiny::getDefaultReactiveDomain())

get_active_pipeline(session = shiny::getDefaultReactiveDomain())
```

## Arguments

- session:

  shiny reactive domain, default is current domain

## Value

A named list, including module ID, module label, internal `'rave_id'`.
