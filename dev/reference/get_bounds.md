# Get bounds of specified limits criteria for different ranks.

Get bounds of specified limits criteria for different ranks.

## Usage

``` r
get_bounds(ls.st, band.lwr, band.upr)
```

## Arguments

- ls.st:

  character vector. students ranks names c("pass", "mid","good",
  "elite")

- band.lwr:

  integer vector. the length must be same as `band.upr`

- band.upr:

  integer vector. the length must be same as `band.upr`

## Value

tibble

## Examples

``` r
st_rank <- c("pass", "mid","good", "elite","mid","pass")
lwr_base <- c(75,80,90,96)
upr_base <- c(79,89,95,98)
tbl_bounds <- get_bounds(ls.st = st_rank,
                         band.lwr = lwr_base,
                         band.upr = upr_base)
#> Warning: `as.tibble()` was deprecated in tibble 2.0.0.
#> ℹ Please use `as_tibble()` instead.
#> ℹ The signature and semantics have changed, see `?as_tibble`.
#> ℹ The deprecated feature was likely used in the xmerit package.
#>   Please report the issue to the authors.
#> Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
#> `.name_repair` is omitted as of tibble 2.0.0.
#> ℹ Using compatibility `.name_repair`.
#> ℹ The deprecated feature was likely used in the tibble package.
#>   Please report the issue at <https://github.com/tidyverse/tibble/issues>.
```
