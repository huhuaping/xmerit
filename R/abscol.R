#' format columns with absolute value and custom digits
#'
#' @param dt data.frame
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
abs.col <-  function(dt, digits){
  dt <- as.data.frame(dt)
  nms <- names(dt)
  nc <- ncol(dt)
  nd <- length(digits)
  if(nc!=nd)
    stop("Argument 'digits' must be vector of length  the number of columns in 'data'.")
  out <- as.data.frame(sapply(1:nc,
                              FUN=function(x, d, Y)
                                formatC(abs(Y[,x]), digits=d[x], format = "f"), Y=dt, d=digits))
  if(!is.null(nms)) names(out) <- nms
  out
}
