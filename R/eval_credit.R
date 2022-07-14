#' Get bounds of specified limits criteria for different ranks.
#'
#' @param ls.st character vector. students ranks names c("pass", "mid","good", "elite")
#' @param band.lwr integer vector. the length must be same as `band.upr`
#' @param band.upr integer vector. the length must be same as `band.upr`
#'
#' @return tibble
#' @export get_bounds
#'
#' @importFrom tibble as.tibble
#' @importFrom dplyr rename_all
#'
#' @examples
#' st_rank <- c("pass", "mid","good", "elite","mid","pass")
#' lwr_base <- c(75,80,90,96)
#' upr_base <- c(79,89,95,98)
#' tbl_bounds <- get_bounds(ls.st = st_rank,
#'                          band.lwr = lwr_base,
#'                          band.upr = upr_base)
get_bounds <-function(ls.st,
                      band.lwr,
                      band.upr){
  ls_full <- c(
    ls.st=="pass",
    ls.st=="mid",
    ls.st=="good",
    ls.st=="elite")

  I0 <- matrix(ls_full,ncol = 4)
  C <- matrix(c(band.lwr, band.upr),
              ncol = 2)
  C0 <- I0 %*% C %>%
    tibble::as.tibble(C0) %>%
    dplyr::rename_all(., ~c("lwr","upr"))
  return(C0)
}


#' Generate score items to satisfy your conditions.
#'
#' @param ls_st character vector. students ranks names c("pass", "mid","good", "elite")
#' @param lwr_base integer vector. the length must be same as `upr_base`
#' @param upr_base integer vector. the length must be same as `lwr_base`
#' @param lwr_lease integer vector. the length must be same as `upr_lease`
#' @param upr_lease integer vector. the length must be same as `lwr_lease`
#' @param seeds integer vector.  random sample seeds.
#' @param wt number vector. weights which the sum equal to 1
#'
#' @return tibble
#' @export get_score
#'
#' @importFrom  purrr map2_int
#' @importFrom  dplyr rename_all
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom scales number
#'
#' @examples
#' myst <- c("pass", "mid","good", "elite","mid","pass")
#' lwr0 <- c(75,80,90,96)
#' upr0 <- c(79,89,95,98)
#' lwr1 <- c(70,77,87,93)
#' upr1 <- c(82,92,97,99)
#' myseeds <- 2341
#' mywt <- c(0.2, 0.8)
#'
#' tbl_score <- get_score(
#'   ls_st = myst,
#'   lwr_base = lwr0,
#'   upr_base = upr0,
#'   lwr_lease = lwr1,
#'   upr_lease = upr1,
#'   seeds = myseeds,
#'   wt = mywt)

get_score <- function(ls_st,
                      lwr_base, upr_base,
                      lwr_lease, upr_lease,
                      seeds, wt){
  C_base <- get_bounds(ls.st = ls_st,
                       band.lwr = lwr_base,
                       band.upr = upr_base)
  # random total score
  P0 <- C_base %>%
    mutate(p0=purrr::map2_int(.x = lwr, .y=upr,
                              ~sample(x =.x:.y, size=1 )))
  # release bounds to practical
  C_lease <- get_bounds(ls.st = ls_st,
                        band.lwr = lwr_lease,
                        band.upr = upr_lease)

  lower <- C_lease$lwr
  upper <- C_lease$upr
  p0 <- P0$p0               # target value
  set.seed(seed = seeds)

  x_out <- NULL
  for (i in 1:nrow(P0)) {
    repeat{
      x1 <- sample(lower[i]:upper[i], size = 1, replace = T)
      x2 <- sample(lower[i]:upper[i], size = 1, replace = T)
      # round() !important
      s <- round(sum(wt[1]*x1, wt[2]*x2),digits = 0)
      if (s == p0[i]) {
        x <- tibble::tibble(s1=x1, s2=x2)
        break
      }
    }
    x_out <- dplyr::bind_rows(x_out, x)
  }

  result <- C_lease %>%
    dplyr::rename_all(., ~c("min","max")) %>%
    dplyr::bind_cols(P0, ., x_out) %>%
    mutate(result = scales::number(wt[1]*s1 +wt[2]*s2, 0.01),
           check = ifelse(round(wt[1]*s1 +wt[2]*s2) == p0, T, F))
  return(result)
}


