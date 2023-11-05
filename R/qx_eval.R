#' Write latex math equation for `lm` (P/S)R(M/F) with initiate value in Quarto style
#'
#' @param lm.mod formula.
#'    you should use `formula()` function
#' @param lm.dt data.frame
#' @param lm.val list. the initiate value list.
#'    you should use the all the variables value. if the model contains the intercept,
#'    then `Intercept = 1` must be included.
#' @param Intercept logical. The default value `TRUE`
#' @param type characters. One of the c('prf', 'srf') with the default value `prf`
#' @param begin numeric. Index number (0 or 1) to set the subscript of the first
#'    greek symbols, with default value: begin =1.
#' @param greek.g character. Specify parameters' Greek symbols,
#'    with default value: greek.g = c("beta").
#' @param lm.n integer. numbers of independent vars of each row in the right equation
#'    with default value 2
#' @param digits integer. list of digits specification on coef result,
#'    with default value 2
#' @param lm.label character. Options for equation label,
#'     default value NULL
#' @param lm.tag character. Options for equation tag,
#'     default value "NULL".
#' @param no_dollar Logistic value. The equation environment
#' should contains double dollars,  with default value "no_dollar = FALSE"
#'
#' @return latexout
#'
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename
#' @importFrom dplyr rename_at
#' @importFrom dplyr rename_all
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr left_join
#' @importFrom tidyr unnest
#' @importFrom stringr str_count
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_c
#' @importFrom purrr map2
#' @importFrom purrr map
#'
#' @export qx.eval
#'
#' @examples
#'
#' data(mtcars)
#' dt_dummy <- mtcars %>%
#'   dplyr::mutate(
#'       gear_3 = ifelse(gear==3, 1, 0),
#'       gear_4 = ifelse(gear==4, 1, 0),    gear_5 = ifelse(gear==5, 1, 0)
#'       ) %>%
#'   dplyr::mutate(
#'       am_1 = ifelse(am==1, 1, 0),
#'       am_0 = ifelse(am==0, 1, 0),
#'       vs_1 = ifelse(vs==1, 1, 0),
#'       vs_0 = ifelse(vs==0, 1, 0) )
#'
#'  mod_prod <- formula(mpg^2 ~ -1 +vs_1 +(gear_4 +gear_5):am_1 +log(wt))
#'  mod_prod <- formula(mpg^2 ~1 + vs_1 +(gear_4 +gear_5):am_1)
#'
#'  fit <- lm(formula = mod_prod, data = dt_dummy)
#'
#'  val_init <- list(
#'    #Intercept = 1,
#'    vs_1 = 0,
#'    gear_4 = 1, gear_5 = 0,
#'    am_1 =1,  "log(wt)" = 1.5
#'    )
#'
#'  qx.out1 <- qx.eval(
#'    lm.mod = mod_prod, lm.dt = dt_dummy,
#'    lm.val = val_init, Intercept = FALSE,
#'    type = "prf",
#'    lm.n = 3, lm.label = "prv-mtcars",
#'    begin =1)
#'  qx.out2 <- qx.eval(
#'    lm.mod = mod_prod, lm.dt = dt_dummy,
#'    lm.val = val_init, Intercept = FALSE,
#'    type = "srf",
#'    lm.n = 3, lm.label = "srv-mtcars")

qx.eval <- function(
    #lm.val,  y = "Y",
    lm.mod, lm.dt,
    lm.val, Intercept = TRUE, type = "prf",
    begin = 1, greek.g = c("beta"),
    lm.n = 2, digits = c(2),
    lm.label =NULL, lm.tag = NULL, no_dollar = FALSE){

  #==== model info ====
  # get the full init names of list
  X_val <- names(lm.val)
  # OLS estimate
  ols.est <- lm(formula = lm.mod, data = lm.dt)
  result <- summary(ols.est)

  # model names of Y
  y <- names(model.frame(ols.est))[1]
  # keep order as the formula,
  ## but this will not contains the intercept whenever you explicit `1` in the formula
  x <-labels(terms(lm.mod, keep.order = TRUE))
  # add the Intercept term
  if (isTRUE(Intercept)) {
    x <- c("(Intercept)", x)
  }

  coef <- as.data.frame(result$coefficients) %>%
    tibble::rownames_to_column(var = "terms") %>%
    .[match(x, .$terms),]  # make sure order and match

  # change terms name
  x.trim <- x %>%
    tibble::as_tibble() %>%
    dplyr::rename("vx"="value") %>%
    mutate(vars = ifelse(str_detect(vx, "^I\\(|Intercept"),
                         str_extract(vx, "(?<=\\()(.+)(?=\\))"),
                         vx)) %>%
    mutate(vars = str_replace(vars, "Intercept", "")) %>%
    mutate(vars = str_replace(vars, "log\\(","ln(")) %>%
    mutate(vars = str_replace_all(vars, " ", ""))

  # tidy the terms and unnest the intersections
  name_new <- c("terms","c", "s", "t", "p")
  df <- coef %>%
    tibble::as_tibble() %>%
    dplyr::rename_at(vars(names(.)), ~name_new) %>%  # rename
    dplyr::bind_cols(x.trim, .)  %>%
    select(-c(terms, s, t, p)) %>%
    mutate(cs = ifelse(c >0, "+", "-"), # get sign
           cv = abs(c)   # absolute value
           )

  # extract X and unnest
  X_val_reg <- X_val %>%
    stringr::str_replace_all(., "\\(",  "\\\\(") %>%
    stringr::str_replace_all(., "\\)",  "\\\\)")

  ptn <- paste0(X_val_reg, collapse = "|")
  tbl_unnest <- df %>%
    mutate(vs = stringr::str_extract_all(vx, ptn)) %>% # extract all X
    tidyr::unnest(cols = vs)

  # construct the init value table
  tbl_val <- lm.val  %>%
    as.data.frame(.) %>%
    tibble::rownames_to_column(var = "vs") %>%
    dplyr::rename_all(., ~c("vs", "value"))

  # match X to terms table
  tbl_match <- dplyr::left_join(tbl_unnest, tbl_val, by = "vs")

  # construct string
  tbl_str <- tbl_match %>%
    mutate(
      value_str = ifelse(
        (value==0 | value ==1), value,
        formatC(value, digits = digits, format = "f"))
      ) %>%
    mutate(value_str = str_c("(", value_str, ")")) %>%
    # group str_c
    dplyr::group_by(vx) %>%
    mutate(str_group = str_c(value_str, collapse = "\\cdot")) %>%
    mutate(value_comb = prod(value)) %>%
    dplyr::ungroup()

  tbl_tidy <- tbl_str %>%
    select(-vs, -value, -value_str) %>%
    dplyr::distinct()  %>%
    mutate(
      value_str = ifelse(
        (value_comb ==1 | value_comb == 0),
        formatC(value_comb , digits = 0, format = "f"),
        formatC(value_comb , digits = digits, format = "f"))
    ) %>%
    mutate(
      par_prefix = ifelse(
        value_comb ==1, "",      # case dummy ==1
        ifelse(
          value_comb ==0, NA,  # case dummy = 0
          value_str              # case continued variables
        )
      )
    )



# ==== latex prepare====
  # set start point and end point
  greek.n <- length(x)
  p_start <- begin
  p_end <- greek.n

  # calculate all cases
  df_n <- tibble::tibble(n=greek.n,
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

  # case if exist intercept or not
  if (isTRUE(Intercept)){
    index <- par_index[-1]
    index_cond <- 2:length(lm.val)   # for conditions
  } else if (!isTRUE(Intercept)) {
    index <- par_index
    index_cond <- 1:length(lm.val) # for conditions
  }


  cond <- paste0(
    paste0(names(lm.val)[index_cond], "=", unlist(lm.val)[index_cond], sep = ""),
    collapse = "; ") %>%
    stringr::str_replace_all(., "\\_", "\\\\_")

  value_cond <- tbl_tidy$str_group  # value of condition
  value_comb <- tbl_tidy$value_str  # combination of the condition values
  coef_est <-  formatC(tbl_tidy$cv, digits = digits, format = "f") # coefs
  coef_sign <- tbl_tidy$cs
  #====left====
  if (type == "prf") {
    left <- paste0("E(",y, "|", cond, ")",collapse = "" )
    par <-  paste0("\\",par_list,"_{", par_index, "}")
    par_val <- paste0(par, value_cond, sep = "" )
    # calculate the estimated value
    par_prefix <- tbl_tidy$par_prefix
    index_null <- which(is.na(par_prefix))
    result <- paste0(par_prefix, par) %>%
      .[-index_null]  %>% # remove the NA element
      paste0(., collapse = " + ")

  } else if (type == "srf") {
    left <- paste0("(\\widehat{", y, "}", "|", cond, ")",collapse = "" )
    par <- paste0("\\hat{\\", par_list,"}_{", par_index, "}")
    par_val <- paste0(par, value_cond, sep = "" )
    est_val <- paste0(coef_sign, "[", coef_est,"]", "\\cdot ",value_cond, sep = "" )
    # calculate the estimated value
    result <- formatC(
      c(t(matrix(tbl_tidy$c)) %*% matrix(tbl_tidy$value_comb)),
      digits = digits, format = "f"
      )
  }


  # ==== body ====
  #lm.n <- 3
  len_x <- length(x)
  breaks <- seq(1, len_x, by = lm.n)
  breaks <- c(breaks, len_x+1)

  right_loop <- NULL
  right_add <- NULL
  #i <-2
  for (i in 1:(length(breaks)-1)){

    range <- breaks[i]:(breaks[i+1]-1)
    right_tem <- paste0("+",par_val[range], collapse = " " )

    if (type == "srf") {
      # add estimated coefs line
      right_tem_add <- paste0(est_val[range], collapse = "" )
    } else if (type == "prf") {
      # no coefs line
      right_tem_add <- NULL
    }

    right_loop <- paste0(right_loop,
                         ifelse(!is.null(right_loop),"\\\\&", ""),
                         right_tem)
    right_add <-  paste0(right_add,
                         ifelse(!is.null(right_add),"\\\\&", ""),
                         right_tem_add)

  }

  if (right_add == "\\\\&") right_add <- "" # case when no need add line

  #==== whole ====

  out_lx <- c(
    ifelse(no_dollar,
           "",
           "$$"),
    "\\begin{aligned}",
    paste0("\\begin{split}"),
    paste0("&", left, "\\\\"),
    paste0("=&", right_loop,  "\\\\"),
    if (type == "srf") {paste0("=&", right_add,  "\\\\")},
    paste0("=&", result),
    "\\end{split}",
    # default no equation tag
    if (!is.null(lm.tag)) {
      paste0('\\quad \\text{(',lm.tag,')}\\quad')},
    "\\end{aligned}",
    # default has double dollar pairs
    ifelse(no_dollar,
           "",
           # default no equation label
           ifelse(is.null(lm.label),
                  "$$",
                  paste0("$$",' {#eq-',lm.label,'}')
           )
    )
  )

  out <- paste0(out_lx, collapse = "\n")

  cat(out_lx, sep = "\n")

  return(out)

}
