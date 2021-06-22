#' Function to extract the overall ANOVA p-value out of a linear model object
#'
#' @param modelobject model object
#' default lm object
#'
#' @export lm.pf
#' @return out
#'
#'



lm.pf <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
