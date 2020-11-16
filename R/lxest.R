#' Write latex math equation of lm estimation for rmarkdown file
#'
#' @param lm.mod character
#' @param lm.dt data.frame
#' @param style character
#' @param obs   character
#' @param opt   character
#' @param lm.label character
#'
#' @return out
#' @export lx.est
#' @import stats
#' @examples
#'
#' lx_out<- lx.est(lm.mod = mod_origin, lm.dt = mroz)
#'
#' mroz <- wooldridge::mroz %>%
#' as_tibble() %>%
#'   select(lwage, educ,exper, fatheduc,motheduc,everything()) %>%
#'              filter(!is.na(wage))
#'
#'  mod_origin <- formula(lwage ~ educ + nwifeinc +exper+I(exper^2) + I(exper^2*city)  )
#'  ols_origin <- lm(formula = mod_origin, data = mroz)
#'
#' lx_out<- lx.est(lm.mod = mod_origin, lm.dt = mroz)
#'
#'
lx.est<- function(lm.mod, lm.dt, style="srf",
                  obs="i", opt=c("s", "t"),lm.label =NULL){
  ols.est <- lm(formula = lm.mod, data = lm.dt)
  result <- summary(ols.est)

  Y <- as.character(lm.mod)[2]
  coef <- result$coefficients
  x <- rownames(coef)

  x.trim <- x %>%
    as_tibble() %>%
    rename("vx"="value") %>%
    mutate(vars = if_else(str_detect(vx, "^I\\(|Intercept"),
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
    abs.col(digits=c(2,4,2,4))

  df.sv<- map2_df(.x = df.sign, .y =  df.val, .f = paste0)

  # here we use the function get.block
  df.cat <- get.block(dt = df.sv, n.row = 3)
  df.x <- bind_cols(x.trim, df.cat)
  # option for the style
  left <- paste0(if_else(style == "srf", "\\widehat{", "{"),
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




