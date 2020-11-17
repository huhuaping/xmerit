#' Write latex math equation of lm estimation for rmarkdown file
#'
#' @param lm.mod character
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
#' @param digits integer.
#' list of digits specification on coef result,
#' with the default value digits=c(2,4,2,4),
#' respectively to c("c","s", "t", "p")
#' @param lm.label character.
#' default value NULL
#'
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
#'     tidyselect::everything())
#'
#' mod_origin <- formula(lwage ~ educ + nwifeinc + exper + I(exper^2) + I(exper^2*city))
#'
#' lx_out <- lx.est(lm.mod = mod_origin, lm.dt = mroz_new)
#'
#'
lx.est<- function(lm.mod, lm.dt, style="srf",
                  obs="i", opt=c("s", "t"), digits=c(2,4,2,4), lm.label =NULL){
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
    abs.col(digits= digits)

  df.sv<- map2_df(.x = df.sign, .y =  df.val, .f = paste0)

  # here we use the function get.block
  df.cat <- get.block(dt = df.sv, n.row = 3)
  df.x <- bind_cols(x.trim, df.cat)
  # option for the style
  left <- paste0(ifelse(style == "srf", "\\widehat{", "{"),
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
    mutate(bx=map(.x = data, .f =function(.x){paste0("&&",unlist(.x), collapse = "")} )) %>%
    select(-data)

  body_main <- rbind(body_hard, body_soft) %>%
    #make sure the order
    mutate(type = factor(type, levels = c("h", opt))) %>%
    arrange(block, type) %>%
    mutate(bx = ifelse((type=="h"&block==1), paste0("&=", bx),
                       ifelse((type=="h"&block!=1), paste0("&(cont.)", bx),
                              ifelse(type!="h",paste0("&(", type,")", bx),
                                     bx)))) %>%
    # option for style
    mutate(bx = ifelse(style=="srm"&type=="h"&block==max(as.numeric(.$block)),
                       paste0(bx, "&&+e_", obs), bx)) %>%
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
    "$$\\begin{equation}",
    str_c('\\begin{alignedat}{',999,"}"),
    whole,
    "\\end{alignedat}",
    # default no equation label
    if (!is.null(lm.label)) {
      paste0('(\\#eq:',lm.label,')')},
    "\\end{equation}$$"
  )

  out <- paste0(out_lx, collapse = "\n")

  cat(out_lx, sep = "\n")

  return(out)
}




