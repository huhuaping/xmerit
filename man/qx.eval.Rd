% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qx_eval.R
\name{qx.eval}
\alias{qx.eval}
\title{Write latex math equation for \code{lm} (P/S)R(M/F) with initiate value in Quarto style}
\usage{
qx.eval(
  lm.mod,
  lm.dt,
  lm.val,
  Intercept = TRUE,
  type = "prf",
  begin = 1,
  greek.g = c("beta"),
  lm.n = 2,
  digits = c(2),
  lm.label = NULL,
  lm.tag = NULL,
  no_dollar = FALSE
)
}
\arguments{
\item{lm.mod}{formula.
you should use \code{formula()} function}

\item{lm.dt}{data.frame}

\item{lm.val}{list. the initiate value list.
you should use the all the variables value. if the model contains the intercept,
then \code{Intercept = 1} must be included.}

\item{Intercept}{logical. The default value \code{TRUE}}

\item{type}{characters. One of the c('prf', 'srf') with the default value \code{prf}}

\item{begin}{numeric. Index number (0 or 1) to set the subscript of the first
greek symbols, with default value: begin =1.}

\item{greek.g}{character. Specify parameters' Greek symbols,
with default value: greek.g = c("beta").}

\item{lm.n}{integer. numbers of independent vars of each row in the right equation
with default value 2}

\item{digits}{integer. list of digits specification on coef result,
with default value 2}

\item{lm.label}{character. Options for equation label,
default value NULL}

\item{lm.tag}{character. Options for equation tag,
default value "NULL".}

\item{no_dollar}{Logistic value. The equation environment
should contains double dollars,  with default value "no_dollar = FALSE"}
}
\value{
latexout
}
\description{
Write latex math equation for \code{lm} (P/S)R(M/F) with initiate value in Quarto style
}
\examples{

data(mtcars)
dt_dummy <- mtcars \%>\%
  dplyr::mutate(
      gear_3 = ifelse(gear==3, 1, 0),
      gear_4 = ifelse(gear==4, 1, 0),    gear_5 = ifelse(gear==5, 1, 0)
      ) \%>\%
  dplyr::mutate(
      am_1 = ifelse(am==1, 1, 0),
      am_0 = ifelse(am==0, 1, 0),
      vs_1 = ifelse(vs==1, 1, 0),
      vs_0 = ifelse(vs==0, 1, 0) )

 mod_prod <- formula(mpg^2 ~ -1 +vs_1 +(gear_4 +gear_5):am_1 +log(wt))
 mod_prod <- formula(mpg^2 ~1 + vs_1 +(gear_4 +gear_5):am_1)

 fit <- lm(formula = mod_prod, data = dt_dummy)

 val_init <- list(
   #Intercept = 1,
   vs_1 = 0,
   gear_4 = 1, gear_5 = 0,
   am_1 =1,  "log(wt)" = 1.5
   )

 qx.out1 <- qx.eval(
   lm.mod = mod_prod, lm.dt = dt_dummy,
   lm.val = val_init, Intercept = FALSE,
   type = "prf",
   lm.n = 3, lm.label = "prv-mtcars",
   begin =1)
 qx.out2 <- qx.eval(
   lm.mod = mod_prod, lm.dt = dt_dummy,
   lm.val = val_init, Intercept = FALSE,
   type = "srf",
   lm.n = 3, lm.label = "srv-mtcars")
}
