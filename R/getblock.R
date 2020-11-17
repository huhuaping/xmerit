#' block the data frame with interval
#'
#' @param dt data.frame
#' @param n.row integer
#'
#' @return out
#' @export get.block
#' @import tidyverse
#' @importFrom dplyr rename_at
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_cols
#' @importFrom tidyselect all_of
#' @importFrom tidyr unnest
#' @importFrom plyr .
#' @importFrom tibble tibble
#' @importFrom tibble add_column
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(u=rnorm(10), v=runif(10, 5,10))
#' out <- get.block(df, 4)
#'
get.block <- function(dt, n.row){
  len.dt <-dim(dt)[1]
  breaks <- seq(1, len.dt, by = n.row)
  breaks <- c(breaks, len.dt+1)
  list.block <- NULL
  for (i in 1:(length(breaks)-1)){
    list.block[[i]] <- c(breaks[i]:(breaks[i+1]-1))
  }

  dt.block <- tibble(list.block) %>%
    rename_at(all_of(names(.)), ~"id") %>%
    add_column(block = 1:nrow(.), .before = "id") %>%
    unnest(id) %>%
    mutate(block = factor(block))

  out <- bind_cols(dt.block, dt)
  return(out)
}
