#' Write latex math equation of lm estimation for rmarkdown file
#'
#' @param lm.mod character.
#' you should use `formula()` function
#' @param lm.dt data.frame
#' @param style character.
#' equation style on c("srf", "srm"),
#' with default value style="srf"
#' @param obs   character.
#' lower script for variables on c("i", "t"),
#'  with default value obs ="i"
#' @param opt   character.
#' list of "soft" option on c("s", "t", "p"),
#' with the default value opt=c("s", "t")
#' @param inf   character.
#' list of "soft" option on c("over","fit","Ftest"),
#' with the default value opt=c("")
#' @param lm.n integer.
#' numbers of independent vars of each row in the right equation.
#' default value lm.n = 3
#' @param digits integer.
#' list of digits specification on coef result,
#' with the default value digits=c(2,4,2,4),
#' respectively to c("c","s", "t", "p")
#' @param lm.label character. Options for equation label,
#'     default value NULL
#' @param lm.tag character. Options for equation tag,
#'     default value "NULL".
#' @param no_dollar Logistic value. The equation environment
#' should contains double dollars,  with default value "no_dollar = FALSE"
#'
#' @return out
#' @export lx.est
#'
#' @import magrittr
#' @import stats
#' @import utils
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom dplyr matches
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr group_nest
#' @importFrom dplyr arrange
#' @importFrom dplyr vars
#' @importFrom tidyr unite
#' @importFrom tidyr gather
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_c
#' @importFrom purrr map2_df
#' @importFrom purrr map
#'
#'
#'
#'
#' @examples
#' require("magrittr")
#' require("wooldridge")
#'
#' mroz_new <- wooldridge::mroz %>%
#'   tibble::as_tibble() %>%
#'   dplyr::select(tidyselect::all_of(c("lwage", "educ", "exper", "fatheduc","motheduc")),
#'     tidyselect::everything()) %>%
#'     dplyr::filter(!is.na(lwage))
#'
#' mod_origin <- formula(lwage ~ educ + nwifeinc + exper + I(exper^2) + I(exper^2*city))
#'
#' lx_out <- lx.est(lm.mod = mod_origin, lm.dt = mroz_new)
#'
#' lx_out2 <- lx.est(lm.mod = mod_origin, lm.dt = mroz_new,
#'   style = c('srm'),inf = c('over','fit','Ftest'),
#'   lm.label = 'test-srm')
#'
lx.est<- function(lm.mod, lm.dt, style="srf",
                  lm.n = 3,
                  obs="i", opt=c("s", "t"),
                  inf = c(""),
                  digits=c(2,4,2,4),
                  lm.label =NULL, lm.tag = NULL,
                  no_dollar = FALSE){
  ols.est <- lm(formula = lm.mod, data = lm.dt)
  result <- summary(ols.est)

  Y <- as.character(lm.mod)[2]
  coef <- result$coefficients
  x <- rownames(coef)

  x.trim <- x %>%
    as_tibble() %>%
    rename("vx"="value") %>%
    mutate(vars = ifelse(str_detect(vx, "^I\\(|Intercept"),
                         str_extract(vx, "(?<=\\()(.+)(?=\\))"),
                         vx)) %>%
    mutate(vars = str_replace(vars, "Intercept", "")) %>%
    mutate(vars = str_replace(vars, "log\\(","ln(")) %>%
    mutate(vars = str_replace_all(vars, " ", ""))

  name_new <- c("c", "s", "t", "p")

  df <- coef %>%
    as_tibble() %>%
    rename_at(vars(names(.)), ~name_new)

  # here we use the function get.sign
  df.sign <- df %>%
    mutate_at(vars(matches("c|t")), .funs =  get.sign) %>%
    mutate_if(is.numeric, .funs = function(.x){.x <- ""} )


  # here we use the function absColumns
  df.val <- df %>%
    absCol(digits= digits)

  df.sv<- map2_df(.x = df.sign, .y =  df.val, .f = paste0)

  # here we use the function get.block
  df.cat <- get.block(dt = df.sv, n.row = lm.n)
  df.x <- bind_cols(x.trim, df.cat)

  # information for model
  n <- stats::nobs(ols.est)
  sigma <- stats::sigma(ols.est)
  R2 <-  result$r.squared
  R2.adj <- result$adj.r.squared
  F.val <- unname(result$fstatistic[1])
  pf <- lm.pf(ols.est) # here use custom fun from lm.pf.R

  val_inf <- c(n, sigma, R2, R2.adj, F.val, pf)
  names_inf <- c("obs", "sigma.hat",
                 "R2", "R2.adj",
                 "F.star", "pf")
  lx_inf <- c("n", "\\hat{\\sigma}",
              "R^2", "\\bar{R}^2",
              "F^*", "p")
  type_inf <- rep(c("over", "fit", "Ftest"), each = 2)


  df.inf <- tibble(type = type_inf,
                   vx = names_inf, lx = lx_inf, value = val_inf) %>%
    add_column(id = 1:nrow(.), .after = "type") %>%
    # format num, use function num_round from utils.R
    mutate(value = dplyr::if_else(vx %in% c("F.star"),
                           num_round(value, 2),
                           ifelse( vx %in% c("obs"), num_round(value, 0),
                                   num_round(value))))

  #inf <- c("over","goodness","Ftest")
  start_b <- max(as.integer(df.cat$block)) +1
  end_b <- start_b + length(inf) -1
  body_inf <- df.inf %>%
    dplyr::filter(type %in% inf) %>%
    tidyr::unite(col = "lx.value", lx, value, sep = "=") %>%
    dplyr::select(type, lx.value) %>%
    mutate(type = factor(type, levels = inf)) %>%
    group_by(type) %>%
    dplyr::group_nest() %>%
    mutate(lx.value=map(.x = data, .f =function(.x){paste0("&&",unlist(.x), collapse = "")} )) %>%
    #mutate(lx.value = paste0("&&", lx.value)) %>%
    dplyr::select(-data) %>%
    dplyr::arrange(type) %>%
    tibble::add_column(block = start_b:end_b, .after = "type") %>%
    dplyr::rename(bx= "lx.value") %>%
    mutate(block = as.factor(block),
           type = as.character(type))



  # option for the style
  left <- paste0(
    ifelse(style == "srf",
           "&\\widehat{",
           "&{"),
    Y,"}")

  body_hard <- df.x %>%
    mutate(vars= ifelse(vx!="(Intercept)",
                        paste0(vars,"_", obs), vars)) %>%
    unite(col = "bx", c, vars, remove = F, sep ="") %>%
    select(block, bx) %>%
    group_by(block) %>%
    group_nest() %>%
    mutate(bx=map(.x = data, .f =function(.x){paste0("&&",unlist(.x), collapse = "")} )) %>%
    select(-data) %>%
    add_column(type ="h", .before = "block")

  opt.pattern <- paste0(opt, collapse = "|")
  body_soft <- df.x %>%
    select(-vars) %>%
    select( block, matches(opt.pattern)) %>%
    gather(key = "type", value = "vale", all_of(opt)) %>%
    group_by(type, block) %>%
    group_nest() %>%
    mutate(bx=map(.x = data, .f =function(.x){paste0("&&(",unlist(.x),")", collapse = "")} )) %>%
    select(-data) %>%
    mutate(bx = as.character(bx))

  body_main <- rbind(body_hard, body_soft, body_inf) %>%
    #make sure the order
    mutate(type = factor(type, levels = c("h", opt, inf))) %>%
    arrange(block, type) %>%
    mutate(bx = ifelse((type=="h"&block==1), paste0("=", bx),
                       ifelse((type=="h"&block!=1), paste0("&(cont.)", bx),
                              ifelse(type!="h",paste0("&(", type,")", bx),
                                     bx)))) %>%
    # option for style
    mutate(bx = ifelse((style%in%"srm")&(dplyr::row_number() %in% max(which(.$type=='h'))),
                       paste0(bx, "\\quad+e_", obs),
                       paste0(bx, "&&"))
           ) %>%
    select(-type) %>%
    # collapse with map
    group_by(block) %>%
    group_nest() %>%
    mutate(bx=map(.x = data, .f =function(.x){paste0(unlist(.x), collapse = "\\\\")} )) %>%
    select(-data)


  body <- body_main %>%
    select(bx) %>%
    unlist() %>%
    paste0( collapse = "\\\\")

  whole <- paste0(left,  body,  collapse = "" )

  out_lx <-c(
    ifelse(no_dollar,
           "\\begin{equation}",
           "$$\\begin{equation}"),
    str_c('\\begin{alignedat}{',999,"}"),
    whole,
    "\\end{alignedat}",
    # default no equation tag
    if (!is.null(lm.tag)) {
      paste0('\\quad \\text{(',lm.tag,')}\\quad')},
    # default no equation label
    if (!is.null(lm.label)) {
      paste0('(\\#eq:',lm.label,')')},
    ifelse(no_dollar,
           "\\end{equation}",
           "\\end{equation}$$")

  )

  out <- paste0(out_lx, collapse = "\n")

  cat(out_lx, sep = "\n")

  return(out)
}




