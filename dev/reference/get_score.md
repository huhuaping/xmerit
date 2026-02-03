# Generate score items to satisfy your conditions.

Generate score items to satisfy your conditions.

## Usage

``` r
get_score(ls_st, lwr_base, upr_base, lwr_lease, upr_lease, seeds, wt)
```

## Arguments

- ls_st:

  character vector. students ranks names c("pass", "mid","good",
  "elite")

- lwr_base:

  integer vector. the length must be same as `upr_base`

- upr_base:

  integer vector. the length must be same as `lwr_base`

- lwr_lease:

  integer vector. the length must be same as `upr_lease`

- upr_lease:

  integer vector. the length must be same as `lwr_lease`

- seeds:

  integer vector. random sample seeds.

- wt:

  number vector. weights which the sum equal to 1

## Value

tibble

## Examples

``` r
myst <- c("pass", "mid","good", "elite","mid","pass")
lwr0 <- c(75,80,90,96)
upr0 <- c(79,89,95,98)
lwr1 <- c(70,77,87,93)
upr1 <- c(82,92,97,99)
myseeds <- 2341
mywt <- c(0.2, 0.8)

tbl_score <- get_score(
  ls_st = myst,
  lwr_base = lwr0,
  upr_base = upr0,
  lwr_lease = lwr1,
  upr_lease = upr1,
  seeds = myseeds,
  wt = mywt)
```
