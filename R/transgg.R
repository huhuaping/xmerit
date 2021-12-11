
#' Translate text from English to Chinese along data frame
#'
#' @param df data frame. The names of columns should be
#'   non-multibytes string.
#' @param col_key character. Which column will be kept as a
#'   "key column", and can be used with data frame join (`left_join()`).
#' @param col_trans character. Which column will be translated.
#' @param ggkey character. google translate API key, which is a long
#'   string containing upper and lower case letters, numbers,
#'   and dashes, such as `a4db08b7-5729-4ba9-8c08-f2df493465a1`.
#'
#' @import translateR
#' @import keyring
#' @import tibble
#'
#' @return result
#' @export gg_translate
#'
#' @examples
#'
#' df_my <- tibble::tibble(
#'   index = 1:3,
#'   target = c(
#'     'We recommend you use the Cloud Console to manage API keys.',
#'     'Your API keys are shown in the API keys section.',
#'     'Pass the API key into a REST API call as a query parameter with the following format.'
#'     )
#'  )
#' \dontrun{
#' result_trans <- gg_translate(
#'   df = df_my,
#'   col_key = "index",
#'   col_trans = "target",
#'   ggkey = keyring::key_get("key",keyring = "gg-trans"))
#' }
#'
gg_translate <- function(df, col_key, col_trans,ggkey){
  names_old <- c(col_key, col_trans)
  names_tem <- c(col_key, "translatedContent")
  names_new <- c(col_key, paste0(col_trans,"_trans"))
  result <- df %>%
    dplyr::select(tidyselect::all_of(names_old)) %>%
    translateR::translate(
      dataset = .,
      content.field = col_trans,
      google.api.key = ggkey,
      source.lang = "en",
      target.lang = "zh-CN")  %>%
    dplyr::select(tidyselect::all_of(names_tem)) %>%
    rename_at(dplyr::vars(tidyselect::all_of(names_tem)),
              ~tidyselect::all_of(names_new))
  return(result)
}

#ggkey = keyring::key_get("key",keyring = "gg-trans")
