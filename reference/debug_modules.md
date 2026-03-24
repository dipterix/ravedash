# Debug 'RAVE' modules interactively in local project folder

Debug 'RAVE' modules interactively in local project folder

## Usage

``` r
debug_modules(
  module_root = rstudioapi::getActiveProject(),
  host = "127.0.0.1",
  port = 17283,
  jupyter = FALSE,
  ...
)
```

## Arguments

- module_root:

  root of modules, usually the project folder created from `'shidashi'`
  template

- host, port:

  host and port of the application

- jupyter:

  whether to launch `'Jupyter'` server; default is false

- ...:

  passed to
  [`render`](https://dipterix.org/shidashi/reference/render.html)

## Value

`'RStudio'` job ID
