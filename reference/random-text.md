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
#> [1] "Please be patient, running in progress..."

finished_text()
#> [1] "Have you finished your coffee? I have finished my work haha."
```
