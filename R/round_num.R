#' formatC for convenient
#'
#' @param num numbers
#' @param digits =4  integer
#'
#' @return out
#'
#'
#' @examples
#'
#' x <- 3.14159
#' out <- round_num(x)
#'
round_num <- function(num, digits = 4) {
  out <- formatC(num, format = "f", digits = digits)
}
