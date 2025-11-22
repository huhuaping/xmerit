# Write latex math equation of lm estimation for Quarto file

Write latex math equation of lm estimation for Quarto file

## Usage

``` r
qx.est(
  lm.mod,
  lm.dt,
  style = "srf",
  lm.n = 3,
  obs = "i",
  opt = c("s", "t"),
  inf = c(""),
  digits = c(2, 4, 2, 4),
  lm.label = NULL,
  lm.tag = NULL,
  no_dollar = FALSE
)
```

## Arguments

- lm.mod:

  formula. you should use
  [`formula()`](https://rdrr.io/r/stats/formula.html) function

- lm.dt:

  data.frame

- style:

  character. equation style on c("srf", "srm"), with default value
  style="srf"

- lm.n:

  integer. numbers of independent vars of each row in the right
  equation. default value lm.n = 3

- obs:

  character. lower script for variables on c("i", "t"), with default
  value obs ="i"

- opt:

  character. list of "soft" option on c("s", "t", "p"), with the default
  value opt=c("s", "t")

- inf:

  character. list of "soft" option on c("over","fit","Ftest"), with the
  default value opt=c("")

- digits:

  integer. list of digits specification on coef result, with the default
  value digits=c(2,4,2,4), respectively to c("c","s", "t", "p")

- lm.label:

  character. Options for equation label, default value NULL

- lm.tag:

  character. Options for equation tag, default value "NULL".

- no_dollar:

  Logistic value. The equation environment should contains double
  dollars, with default value "no_dollar = FALSE"

## Value

out

## Examples

``` r
library(wooldridge)
data(mroz)
mroz_new <- mroz %>%
  tibble::as_tibble() %>%
  dplyr::select(tidyselect::all_of(c("lwage", "educ", "exper", "fatheduc","motheduc")),
    tidyselect::everything()) %>%
    dplyr::filter(!is.na(lwage)) %>%
    dplyr::rename('educ_p_q'='educ')

mod_origin <- formula(lwage ~ educ_p_q + nwifeinc + exper + I(exper^2) + I(exper^2*city))

px_out <- qx.est(lm.mod = mod_origin, lm.dt = mroz_new)
#> $$
#> \begin{alignedat}{999}
#> \begin{split}
#> &\widehat{lwage}=&&-0.53&&+0.10educ\_p\_q_i&&+0.01nwifeinc_i\\ 
#> &(s)&&(0.2011)&&(0.0148)&&(0.0032)\\ 
#> &(t)&&(-2.61)&&(+6.67)&&(+1.59)\\ 
#> &(cont.)&&+0.04exper_i&&-0.00exper^2_i&&+0.00exper^2*city_i\\ 
#> &(s)&&(0.0132)&&(0.0004)&&(0.0002)\\ 
#> &(t)&&(+3.23)&&(-2.19)&&(+0.79)
#> \end{split}
#> \end{alignedat}
#> $$

px_out2 <- qx.est(lm.mod = mod_origin, lm.dt = mroz_new,
  style = c('srf'),inf = c('over','fit','Ftest'),
  lm.label = 'test-srm')
#> $$
#> \begin{alignedat}{999}
#> \begin{split}
#> &\widehat{lwage}=&&-0.53&&+0.10educ\_p\_q_i&&+0.01nwifeinc_i\\ 
#> &(s)&&(0.2011)&&(0.0148)&&(0.0032)\\ 
#> &(t)&&(-2.61)&&(+6.67)&&(+1.59)\\ 
#> &(cont.)&&+0.04exper_i&&-0.00exper^2_i&&+0.00exper^2*city_i\\ 
#> &(s)&&(0.0132)&&(0.0004)&&(0.0002)\\ 
#> &(t)&&(+3.23)&&(-2.19)&&(+0.79)\\ 
#> &(over)&&n=428&&\hat{\sigma}=0.6653 &&\\ 
#> &(fit)&&R^2=0.1636&&\bar{R}^2=0.1537 &&\\ 
#> &(Ftest)&&F^*=16.51&&p=0.0000 &&
#> \end{split}
#> \end{alignedat}
#> $$ {#eq-test-srm}
```
