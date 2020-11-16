
#' get the plus and minus symbols
#'
#' @param df numeric
#'
#' @return out
#' @export get.sign
#'
#' @examples
#' set.seed(123)
#' v <- rnorm(10)
#' get.sign(v)
#'
#'
get.sign <- function(df){
  out <- ifelse(df<0 , "-", "+")
  return(out)
}
