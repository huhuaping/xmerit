#' format columns with absolute value and custom digits
#'
#' @param x data.frame
#' @param digits list
#'
#' @return out
#' @export abs.col
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(u=rnorm(10), v=runif(10, 5,10))
#' out <- abs.col(df, c(4,2))
#'
#'
abs.col <-  function(x, digits){
  x <- as.data.frame(x)
  nms <- names(x)
  nc <- ncol(x)
  nd <- length(digits)
  if(nc!=nd)
    stop("Argument 'digits' must be vector of length  the number of columns in 'data'.")
  out <- as.data.frame(sapply(1:nc,
                              FUN=function(y, d, Z)
                                formatC(abs(Z[,y]), digits=d[y], format = "f"), Z=x, d=digits))
  if(!is.null(nms)) names(out) <- nms
  out <- tibble::as_tibble(out)
  return(out)
}
