# Randomly choose a text from a list of strings

Randomly choose a text from a list of strings

## Usage

``` r
be_patient_text(candidates)

finished_text(candidates)
```

## Arguments

- candidates:

  character vectors, a list of candidates

## Value

`be_patient_text` returns a text asking users to be patient;
`finished_text` returns the text indicating the task has finished.

## Examples

``` r
be_patient_text()
#> [1] "Grab a cup of coffee, this might take a while... (Can I get the decaf one? Thanks)"

finished_text()
#> [1] "Please proceed to the next step"
```
