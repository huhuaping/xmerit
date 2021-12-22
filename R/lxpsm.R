#' Write latex math equation of econometrics text for rmarkdown file
#'
#'
#' @param x character. Vector of all independent variables.
#' @param y character. The dependent variables.
#' @param intercept logical. Model intercept, with default value: TRUE.
#' @param  begin numeric. Index number (0 or 1) to set the subscript of the first
#'   greek symbols, with default value: begin =1.
#' @param greek.g character. Specify parameters' Greek symbols,
#'     with default value: greek.g = c("beta").
#' @param greek.n integer. Specify the number respect to  "greek.g" vector,
#'     and the default value is: "greek.n = length(x)+1".
#' @param type    character. Types of model, with options
#'     type=c("prm","prf","srf","srm").
#' @param lm.label character. Options for equation label, default value "NULL".
#' @param lm.tag character. Options for equation tag, default value "NULL".
#' @param obs character. options for subscript, with options "obs = c('i', 't')",
#'     and the default value is : obs = 'i'.
#' @param n.row integer. Numbers of variables in each row, default value 2
#' @param no_dollar Logistic value. The equation environment
#' should contains double dollars,  with default value "no_dollar = FALSE"
#'
#'
#' @importFrom magrittr %>%
#' @import tidyverse
#' @return out
#' @export lx.psm
#' @examples
#' X <- c(paste0(rep(c("X","Z"),each=4),1:4), "fathedu", "mothedu")
#' Y <- "lwage"
#' Greek.g <- c("alpha","beta","lambda")
#' Greek.n <- c(4,4,2)
#' Greek.n <- c(4,4,2)
#' Obs <- "i"
#' N.row <- 5
#' Cst <- TRUE
#'
#' out <- lx.psm(x =X, y = Y, begin =1,
#'   greek.g = Greek.g, greek.n = Greek.n,
#'   type = "prm", intercept = Cst, lm.label = "prm",
#'   obs = Obs, n.row = N.row )
#'



lx.psm <- function(x, y = "Y",
                   intercept = TRUE, begin =1,
                   greek.g = c("beta"), greek.n = length(x)+1,
                   type = "prm", lm.label=NULL, lm.tag = NULL,
                   obs ="i",n.row=2,
                   no_dollar = FALSE){

  # set start point and end point
  if (begin ==0) {
    p_start <- 0
    if (intercept==FALSE){
      p_end <- greek.n[1]-1
    } else if(intercept==TRUE){
      p_end <- greek.n[1]
      x <- c("", x)
    }
  } else {
    p_start <- 1
    if (intercept==FALSE){
      p_end <- greek.n[1]
    } else if(intercept==TRUE){
      p_end <- greek.n[1] +1
      x <- c("", x)
    }
  }

  x<- stringr::str_replace_all(x, "\\_", "\\\\_")


  # calculate all cases
  df_n <- tibble(n=greek.n,
                 part =paste0("P",1:length(greek.n))) %>%
   mutate(start = ifelse(part %in%c("P1"),
                          p_start, 1),
           end = ifelse(part %in%c("P1"),
                        p_end, n)) %>%
    mutate(n_total = end-start +1) %>%
    mutate(index =purrr::map2(.x = start,
                       .y =end,
                       .f = function(x, y)seq(x,y)) )
  # get list of index and greek symbols
  par_index <- unlist(df_n$index)
  par_list <- rep(greek.g, times= df_n$n_total)


  left <- stringr::str_replace(y,"\\_","\\\\_")
  tail <- paste0("u_", obs)
  par <-  paste0("\\",par_list,"_{", par_index, "}")


  if (type == "prf") {
    check <- which(x=="")
    if (length(check) == 0){
      x.trim <- x
    } else {
      x.trim <- x[-which(x=="")]
    }

    left <- paste0("E(",y, "|", paste0(x.trim, collapse = ","), ")",collapse = "" )
    tail <- NA
  } else if (type == "srm") {
    par <- paste0("\\hat{\\", par_list,"}_{", par_index, "}")
    tail <- paste0("e_", obs)
  } else if (type == "srf") {
    left <- paste0("\\widehat{", y, "}")
    par <- paste0("\\hat{\\", par_list,"}_{", par_index, "}")
    tail <- NA
  }

  #n.row <- 3

  x.trim <- ifelse(x=="", x, paste0(x, "_", obs))

  len_x <- length(x.trim)
  breaks <- seq(1, len_x, by = n.row)
  breaks <- c(breaks, len_x+1)

  right_loop <- NULL
  #i <-2
  for (i in 1:(length(breaks)-1)){

    range <- breaks[i]:(breaks[i+1]-1)

    right_tem <- paste0("+",par[range], x.trim[range], collapse = "" )

    right_loop <- paste0(right_loop,
                         ifelse(!is.null(right_loop),"\\\\&", ""),
                         right_tem)
  }


  body_px <- paste0("&",right_loop)

  if(!is.na(tail)){
    right <- paste0(body_px, "+",tail)
  }  else {
    right <- body_px
  }


  whole <- paste0(left,
                  ifelse(type=="prf","",
                          paste0("_",obs)),
                  "=", right,  collapse = "" )

  out_lx <- c(
    ifelse(no_dollar,
           "\\begin{align}",
           "$$\\begin{align}"),
    paste0("\\begin{split}"),
    whole,
    "\\end{split}",
    # default no equation tag
    if (!is.null(lm.tag)) {
      paste0('\\quad \\text{(',lm.tag,')}\\quad')},
    # default no equation label
    if (!is.null(lm.label)) {
      paste0('(\\#eq:',lm.label,')')},
    ifelse(no_dollar,
           "\\end{align}",
           "\\end{align}$$")
  )

  out <- paste0(out_lx, collapse = "\n")

  cat(out_lx, sep = "\n")

  return(out)
}
