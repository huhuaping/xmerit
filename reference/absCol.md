# format columns with absolute value and custom digits

format columns with absolute value and custom digits

## Usage

``` r
absCol(x, digits)
```

## Arguments

- x:

  data.frame

- digits:

  list

## Value

out

## Examples

``` r
set.seed(123)
df <- data.frame(u=rnorm(10), v=runif(10, 5,10))
out <- absCol(df, c(4,2))

```
