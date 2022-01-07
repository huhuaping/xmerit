#' Give needed 'nbsp' symbols, and auto split with nice alignment.
#'
#' @param len number, specified total length of one line. The default is "len=50".
#' @param n_trim number, number of Chinese characters need to  trim.
#' @param ratio integer, ratio of one Chinese characters to one nbsp. The default is "len=4".
#'
#' @return list
#' @export give_nbsp
#'
#' @examples
#'
#' give_nbsp(n_trim = c(10,5))


give_nbsp <- function(n_trim, len=50, ratio=4){
  lens <-len*ratio
  n_trim <- n_trim*ratio
  num_parts <- length(n_trim)
  n_avr <- floor(lens/num_parts)
  n_add <- n_avr - n_trim
  out <- sapply(n_add,FUN = function(x){paste0(rep("&nbsp;",x),collapse='')})
  return(out)
}

