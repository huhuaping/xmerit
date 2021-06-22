#' formatC for convenient
#'
#' @param num numbers
#' @param digits integer
#' default digits = 4
#'
#' @export num_round
#' @return out
#'
#'
#' @examples
#'
#' x <- 3.14159
#' out <- num_round(x)
#'
num_round <- function(num, digits = 4) {
  out <- formatC(num, format = "f", digits = digits)
}
