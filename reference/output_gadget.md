# 'RAVE' dashboard output gadgets

'RAVE' dashboard output gadgets

## Usage

``` r
output_gadget(
  outputId,
  icon = NULL,
  type = c("standalone", "download", "download2", "actionbutton", "custom"),
  class = NULL,
  inputId = NULL,
  ...
)

output_gadget_container(
  expr,
  gadgets = c("standalone", "download2"),
  quoted = FALSE,
  env = parent.frame(),
  outputId = NULL,
  class = NULL,
  container = NULL,
  wrapper = TRUE
)
```

## Arguments

- outputId:

  output ID in the root scope of shiny session

- icon:

  gadget icon

- type, gadgets:

  gadget type(s), currently supported: `'standalone'`, `'download'`,
  `'actionbutton'`

- class:

  additional class to the gadget or its container

- inputId:

  input ID, automatically assigned internally

- ...:

  ignored

- expr:

  shiny output call expression, for example, `shiny::plotOutput({...})`

- quoted:

  whether `expr` is quoted; default is false

- env:

  environment where `expr` should be evaluated

- container:

  optional container for the gadgets and outputs; will be ignored if
  `wrapper` is false

- wrapper:

  whether to wrap the gadgets and the output within a 'HTML' container
