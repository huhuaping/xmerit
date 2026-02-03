# Write latex math equation for `lm` (P/S)R(M/F) in Quarto style

Write latex math equation for `lm` (P/S)R(M/F) in Quarto style

## Usage

``` r
qx.psm(
  x,
  y = "Y",
  intercept = TRUE,
  begin = 1,
  greek.g = c("beta"),
  greek.n = length(x) + 1,
  type = "prm",
  lm.label = NULL,
  lm.tag = NULL,
  obs = "i",
  u = "u",
  n.row = 2,
  no_dollar = FALSE
)
```

## Arguments

- x:

  character. Vector of all independent variables.

- y:

  character. The dependent variables.

- intercept:

  logical. Model intercept, with default value: TRUE.

- begin:

  numeric. Index number (0 or 1) to set the subscript of the first greek
  symbols, with default value: begin =1.

- greek.g:

  character. Specify parameters' Greek symbols, with default value:
  greek.g = c("beta").

- greek.n:

  integer. Specify the number respect to "greek.g" vector, and the
  default value is: "greek.n = length(x)+1".

- type:

  character. Types of model, with options
  type=c("prm","prf","srf","srm").

- lm.label:

  character. Options for equation label, default value "NULL".

- lm.tag:

  character. Options for equation tag, default value "NULL".

- obs:

  character. options for subscript, with options "obs = c('i', 't')",
  and the default value is : obs = 'i'.

- u:

  character. options for the disturbance term with default `u = "u"`.

- n.row:

  integer. Numbers of variables in each row, default value 2

- no_dollar:

  Logistic value. The equation environment should contains double
  dollars, with default value "no_dollar = FALSE"

## Value

out

## Examples

``` r
X <- c(paste0(rep(c("X","Z"),each=4),1:4), "fathedu", "mothedu")
Y <- "lwage"
Greek.g <- c("alpha","beta","lambda")
Greek.n <- c(4,4,2)
Greek.n <- c(4,4,2)
Obs <- "i"
N.row <- 5
Cst <- TRUE

out <- qx.psm(x =X, y = Y, begin =1,
  greek.g = Greek.g, greek.n = Greek.n,
  type = "prm", intercept = Cst, lm.label = "prm",
  obs = Obs, n.row = N.row )
#> $$
#> \begin{aligned}
#> \begin{split}
#> lwage_i=&+\alpha_{1}+\alpha_{2}X1_i+\alpha_{3}X2_i+\alpha_{4}X3_i+\alpha_{5}X4_i\\&+\beta_{1}Z1_i+\beta_{2}Z2_i+\beta_{3}Z3_i+\beta_{4}Z4_i+\lambda_{1}fathedu_i\\&+\lambda_{2}mothedu_i+u_i
#> \end{split}
#> \end{aligned}
#> $$ {#eq-prm}
```
