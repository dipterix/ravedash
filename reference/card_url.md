# Set 'URL' scheme for modules

Automatically generates `href` for
[`input_card`](https://dipterix.org/ravedash/reference/rave-input-output-card.md)
and
[`output_card`](https://dipterix.org/ravedash/reference/rave-input-output-card.md)

## Usage

``` r
set_card_url_scheme(module_id, root, sep = "/")

card_href(title, type = "input", module_id = NULL)
```

## Arguments

- module_id:

  the module ID

- root:

  'URL' default route

- sep:

  separation

- title:

  a title string that will be used to generate 'URL'

- type:

  type of the card; choices are `'input'` or `'output'`

## Value

The hyper reference of suggested card 'URL'

## Examples

``` r
set_card_url_scheme(
  module_id = "power_explorer",
  root = "https://openwetware.org/wiki/RAVE:ravebuiltins",
  sep = ":")
#> [1] "https://openwetware.org/wiki/RAVE:ravebuiltins:power_explorer:<card type>_<Placeholder for card title>"

card_href("Set Electrodes", type = "input", module_id = "power_explorer")
#> [1] "https://openwetware.org/wiki/RAVE:ravebuiltins:power_explorer:input_setelectrodes"

```
