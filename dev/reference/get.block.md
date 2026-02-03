# block the data frame with interval

block the data frame with interval

## Usage

``` r
get.block(dt, n.row)
```

## Arguments

- dt:

  data.frame

- n.row:

  integer

## Value

out

## Examples

``` r
set.seed(123)
df <- data.frame(u=rnorm(10), v=runif(10, 5,10))
out <- get.block(df, 4)
```
