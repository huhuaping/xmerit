% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qxpsm.R
\name{qx.psm}
\alias{qx.psm}
\title{Write latex math equation for \code{lm} (P/S)R(M/F) in Quarto style}
\usage{
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
}
\arguments{
\item{x}{character. Vector of all independent variables.}

\item{y}{character. The dependent variables.}

\item{intercept}{logical. Model intercept, with default value: TRUE.}

\item{begin}{numeric. Index number (0 or 1) to set the subscript of the first
greek symbols, with default value: begin =1.}

\item{greek.g}{character. Specify parameters' Greek symbols,
with default value: greek.g = c("beta").}

\item{greek.n}{integer. Specify the number respect to  "greek.g" vector,
and the default value is: "greek.n = length(x)+1".}

\item{type}{character. Types of model, with options
type=c("prm","prf","srf","srm").}

\item{lm.label}{character. Options for equation label, default value "NULL".}

\item{lm.tag}{character. Options for equation tag, default value "NULL".}

\item{obs}{character. options for subscript, with options "obs = c('i', 't')",
and the default value is : obs = 'i'.}

\item{u}{character. options for the disturbance term with default \code{u = "u"}.}

\item{n.row}{integer. Numbers of variables in each row, default value 2}

\item{no_dollar}{Logistic value. The equation environment
should contains double dollars,  with default value "no_dollar = FALSE"}
}
\value{
out
}
\description{
Write latex math equation for \code{lm} (P/S)R(M/F) in Quarto style
}
\examples{
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

}
