#' Write latex math equation for rmarkdown file
#'
#' @param x character
#' @param y character
#' @param intercept logical
#' @param greek.g character
#' @param greek.n integer
#' @param type    character
#' @param lm.label character
#' @param obs character
#' @param n.row integer
#' @importFrom magrittr %>%
#' @import tidyverse
#'
#' @examples
#' X <- c(paste0(rep(c("X","Z"),each=4),1:4), "fathedu", "mothedu")
#' Y <- "lwage"
#' Greek.g <- c("alpha","beta","lambda")
#' Greek.n <- c(4,4,2)
#' Greek.n <- c(4,4,2)
#' Obs <- "i"
#' N.row <- 5
#' Cst <- FALSE
#'
#' lx.psm(x =X, y = Y,greek.g = Greek.g, greek.n = Greek.n,
#'   type = "prm", intercept = Cst , lm.label = "prm",
#'   obs = Obs, n.row = N.row )
#'



lx.psm <- function(x, y = "Y", intercept = TRUE,
                   greek.g = c("beta"), greek.n,
                   type = "prm", lm.label=NULL, obs ="i", n.row){

  par_index <- lapply(greek.n, FUN = function(x) 1:x)  %>%
    unlist()

  greek.n.tem <-  greek.n
  if (isTRUE(intercept)){
    par_index <- c(0, par_index)
    greek.n.tem[1] <- greek.n[1] +1
    x <- c("", x)
  } else {
    par_index <- c( par_index)
    greek.n.tem <- greek.n
  }

  par_list <- rep(greek.g, times= greek.n.tem)



  left <- y
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

    right_tem <- paste0("&&",par[range], x.trim[range], collapse = "+" )

    right_loop <- paste0(right_loop,
                         ifelse(!is.null(right_loop),"\\\\&+", ""),
                         right_tem)
  }


  body_px <- right_loop

  if(!is.na(tail)){
    right <- paste0(body_px, "+&&",tail)
  }  else {
    right <- body_px
  }


  whole <- paste0(left,
                  ifelse(type=="prf","",
                          paste0("_",obs)),
                  "&=", right,  collapse = "" )

  cat(
    "$$\\begin{equation}",
    paste0('\\begin{alignedat}{',999,"}"),
    whole,
    "\\end{alignedat}",
    # default no equation label
    if (!is.null(lm.label)) {
      paste0('(\\#eq:',lm.label,')')},
    "\\end{equation}$$",
    sep = "\n"
  )

}
