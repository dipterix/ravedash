# Obtain caching object for current run-time shiny session

Cache small objects such as inputs or configurations

## Usage

``` r
shiny_cache(namespace, session = shiny::getDefaultReactiveDomain())
```

## Arguments

- namespace:

  characters, usually the module ID

- session:

  shiny interactive context domain

## Value

A caching object. The caching object is identical within the same
context and namespace.
