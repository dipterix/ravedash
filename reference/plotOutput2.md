# Shiny plot output with minimum height and additional classes

Shiny plot output with minimum height and additional classes

## Usage

``` r
plotOutput2(
  outputId,
  class = NULL,
  width = "100%",
  height = "100%",
  min_height = "400px",
  ...
)
```

## Arguments

- outputId, width, height, ...:

  passed to
  [`plotOutput`](https://rdrr.io/pkg/shiny/man/plotOutput.html)

- class:

  additional 'HTML' class of the output wrapper

- min_height:

  minimum height of the image; default is 400 pixels

## Value

A plot output element that can be included in a panel.

## Examples

``` r
plotOutput2("plot", class = "rounded overflow-hidden",
            min_height = 300)
#> <div class="shiny-plot-output rounded overflow-hidden html-fill-item" id="plot" style="width:100%;height:100%;;min-height:300px"></div>

```
