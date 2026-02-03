# =============================================================================
# 问卷题项选项行生成 — 用于 Quarto qmd（HTML / Word 双格式）
# 方案一：R 函数生成整行选项，支持勾选框与字母 A/B/C/D 两种样式
# 勾选框：HTML 下为 <input type="checkbox">，Word 下为 Unicode ☐ (U+2610)
# 支持选项过多或选项文字较长时按 A4 宽度自动换行
# =============================================================================
#
# 在 qmd 中使用方法：
# 1. 在文档开头增加 R 代码块加载本脚本，例如：
#    ```{r setup, include=FALSE}
#    source(here::here("R", "questionnaire-opt.R"))
#    ```
# 2. 题干保持一行，选项行改为内联 R 调用，例如：
#    1.您的性别：
#
#    `r opt_line(c("男", "女"))`
#    或使用字母选项：`r opt_line(c("男", "女"), style = "letter")`
#
# 3. 选项多或文字长时启用换行（适配 A4）：
#    `r opt_line(c("选项1", "选项2", ...), wrap = TRUE, max_width = 42)`
#
# 4. 可选：在 setup 中设置默认样式，如
#    opt_line_default <- function(...) opt_line(..., style = "letter")
#
# 5. 可选：在 setup 中设置默认字母选项或勾选框样式，如
#    options(questionnaire.opt_style = "letter")  # 默认字母选项样式
#    options(questionnaire.opt_style = "checkbox")  # 默认勾选框样式
# =============================================================================

#' Display width of a string (for A4 line wrapping)
#'
#' CJK characters count as 2 units; ASCII letters, digits and half-width
#' symbols count as 1 unit, consistent with common typesetting.
#'
#' @param s A single string.
#' @return Integer, width in half-width units.
#' @noRd
str_display_width <- function(s) {
  if (is.na(s) || length(s) == 0L || nchar(s) == 0L) return(0L)
  chars <- strsplit(s, "")[[1]]
  if (length(chars) == 0L) return(0L)
  w <- 0L
  for (i in seq_along(chars)) {
    code <- utf8ToInt(chars[i])
    if (length(code) != 1L) { w <- w + 1L; next }
    # CJK 统一汉字、CJK 标点、全角符号、假名等计为 2
    if (code >= 0x4E00L && code <= 0x9FFFL) { w <- w + 2L; next }
    if (code >= 0x3000L && code <= 0x303FL) { w <- w + 2L; next }
    if (code >= 0xFF00L && code <= 0xFFEFL) { w <- w + 2L; next }
    if (code >= 0x3040L && code <= 0x30FFL) { w <- w + 2L; next }
    w <- w + 1L
  }
  w
}

#' Detect if current output format is HTML (for checkbox/format branching)
#'
#' @return Logical. \code{TRUE} if output is HTML.
#' @noRd
opt_is_html <- function() {
  tryCatch(
    knitr::is_html_output(excludes = c("markdown", "epub")),
    error = function(e) FALSE
  )
}

#' Escape option text for HTML (ampersand, less-than, greater-than)
#'
#' @param s Character vector (single element used).
#' @return Character string, HTML-safe.
#' @noRd
escape_html <- function(s) {
  if (!is.character(s) || length(s) == 0L) return("")
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s
}

#' Option gap string depending on output format
#'
#' HTML uses \code{&nbsp;} entity, Word uses Unicode no-break space (U+00A0),
#' so spacing is consistent in both formats.
#'
#' @param n Number of gap units (default 4).
#' @return Length-1 character vector to paste into option line.
#' @noRd
opt_gap <- function(n = 4L) {
  n <- as.integer(n)
  if (n < 0L) n <- 0L
  is_html <- opt_is_html()
  space_char <- if (is_html) "&nbsp;" else "\u00A0"  # Unicode 不换行空格
  paste(rep(space_char, n), collapse = "")
}

#' Display width of option gap (for line-wrap calculation)
#'
#' Gap from \code{opt_gap(n)} renders as n no-break spaces; display width
#' is n half-width units. Used when \code{gap_n} increases and fewer options
#' fit per line.
#'
#' @param n Number of gap units (same as \code{gap_n}).
#' @return Integer, width in half-width units.
#' @noRd
opt_gap_display_width <- function(n) {
  n <- as.integer(n)
  if (n < 0L) n <- 0L
  n
}

#' Convert indices to letter labels (1->A, 2->B, ..., 26->Z, 27->AA, ...)
#'
#' @param i Integer vector of positive indices.
#' @return Character vector of letter labels.
#' @noRd
idx_to_letter <- function(i) {
  i <- as.integer(i)
  out <- character(length(i))
  for (k in seq_along(i)) {
    if (i[k] <= 26L) {
      out[k] <- LETTERS[i[k]]
    } else {
      # 27 -> AA, 28 -> AB, ...
      q <- (i[k] - 1L) %/% 26L
      r <- (i[k] - 1L) %% 26L + 1L
      out[k] <- paste0(LETTERS[q], LETTERS[r])
    }
  }
  out
}

#' Generate one or more lines of questionnaire option items
#'
#' Produces a single line (or wrapped lines) of options for questionnaire
#' items in Quarto qmd, supporting HTML and Word (docx). Format is chosen
#' automatically from the current Quarto output.
#'
#' @section Styles:
#' \itemize{
#'   \item \strong{Checkbox} (\code{style = "checkbox"}): In HTML, outputs
#'   \code{<input type="checkbox">}; in Word, outputs Unicode ballot box
#'   (U+2610) for print-and-fill.
#'   \item \strong{Letter} (\code{style = "letter"}): Labels like
#'   \code{A.}, \code{B.}, \code{C.}.
#' }
#'
#' @section Line wrapping:
#' When \code{wrap = TRUE}, options are wrapped by display width
#' (\code{max_width} in half-width units). CJK characters count as 2 units,
#' ASCII as 1, for A4-friendly layout.
#'
#' @param options Character vector of option labels (e.g. \code{c("Yes", "No")}).
#' @param gap_n Number of gap units between options (default 8). HTML uses
#'   \code{&nbsp;}, Word uses Unicode no-break space.
#' @param style \code{"checkbox"} or \code{"letter"}. If \code{NULL}, uses
#'   \code{getOption("questionnaire.opt_style", "checkbox")} so you can set
#'   \code{options(questionnaire.opt_style = "letter")} in qmd setup.
#' @param letter_sep Separator between letter and text when \code{style = "letter"}
#'   (e.g. \code{". "} or \code{", "}).
#' @param wrap If \code{TRUE}, wrap by \code{max_width}; use with \code{indent_wrap}.
#' @param max_width Maximum display width per line in half-width units (default 42).
#' @param indent_wrap Indent string for continuation lines (default 3 spaces).
#' @return Length-1 character vector (raw HTML when checkbox + HTML, via
#'   \code{knitr::asis_output}). Use in qmd inline R, e.g. \code{opt_line(...)}.
#'
#' @export
#' @importFrom knitr is_html_output asis_output
#' @examples
#' # Checkbox style (default)
#' opt_line(c("Male", "Female"))
#' opt_line(c("Very much", "Somewhat", "Neutral", "A little", "Not at all"))
#'
#' # Line wrapping for long options (A4)
#' opt_line(c("Too expensive", "No need", "Good health"),
#'          wrap = TRUE, max_width = 42)
#'
#' # Letter style
#' opt_line(c("Yes", "No"), style = "letter")
#' opt_line(c("Yes", "No"), style = "letter", letter_sep = ". ")
opt_line <- function(options,
                    gap_n = 8L,
                    style = NULL,
                    letter_sep = ". ",
                    wrap = FALSE,
                    max_width = 42L,
                    indent_wrap = "   ") {
  if (!is.character(options) || length(options) == 0L) {
    return("")
  }
  options <- as.character(options)  # 将选项转换为字符串向量
  if (is.null(style)) style <- getOption("questionnaire.opt_style", "checkbox") 
  style <- match.arg(style, c("checkbox", "letter"))
  is_html <- opt_is_html()
  gap <- opt_gap(gap_n)
  gap_n <- as.integer(gap_n)
  max_width <- as.integer(max_width)
  if (max_width < 1L) max_width <- 42L

  if (style == "checkbox") {
    # HTML：真实 <input type="checkbox">（可交互勾选）；Word：Unicode ☐ (U+2610) 方框符号，便于打印填写
    if (is_html) {
      parts <- vapply(options, function(o) {
        paste0('<span class="q-opt"><input type="checkbox" class="q-opt-cb"> ', escape_html(o), '</span>')
      }, character(1))
    } else {
      parts <- paste0("\u2610 ", options)  # Unicode BALLOT BOX (empty checkbox)
    }
  } else {
    letters <- idx_to_letter(seq_along(options))
    parts <- paste0(letters, letter_sep, options)
  }

  if (!isTRUE(wrap)) {
    result <- paste(parts, collapse = gap)
    if (style == "checkbox" && is_html) return(knitr::asis_output(result))
    return(result)
  }

  # 按宽度换行：总宽度 = 各 part 显示宽度之和 + (选项数-1)*间隔显示宽度；gap_n 增大时间隔变长，需显式计入
  gap_display_width <- opt_gap_display_width(gap_n)
  if (style == "checkbox") {
    part_widths <- vapply(options, function(o) str_display_width(paste0("\u2610 ", o)), integer(1))
  } else {
    part_widths <- vapply(parts, str_display_width, integer(1))
  }
  indent_width <- str_display_width(indent_wrap)

  lines <- character(0L)
  current_line <- character(0L)
  current_width <- 0L
  first_in_line <- TRUE

  for (i in seq_along(parts)) {
    would_be <- current_width + if (first_in_line) part_widths[i] else (gap_display_width + part_widths[i])

    if (!first_in_line && would_be > max_width) {
      # 当前行已放不下，先保存当前行，再新起一行（新行带缩进宽度）
      lines <- c(lines, paste(current_line, collapse = gap))
      current_line <- parts[i]
      current_width <- indent_width + part_widths[i]
      first_in_line <- FALSE
    } else {
      if (first_in_line) {
        current_line <- parts[i]
        current_width <- part_widths[i]
        first_in_line <- FALSE
      } else {
        current_line <- c(current_line, parts[i])
        current_width <- current_width + gap_display_width + part_widths[i]
      }
    }
  }
  if (length(current_line) > 0L) {
    lines <- c(lines, paste(current_line, collapse = gap))
  }

  # 从第二行起加缩进；HTML 勾选框用 <br> 换行（raw HTML），否则用 Markdown 软换行
  if (length(lines) <= 1L) {
    result <- paste(lines, collapse = "")
    if (style == "checkbox" && is_html) return(knitr::asis_output(result))
    return(result)
  }
  if (style == "checkbox" && is_html) {
    result <- paste(lines[1], paste0(indent_wrap, lines[-1], collapse = "<br>\n"), sep = "<br>\n")
    return(knitr::asis_output(result))
  }
  result <- paste(
    lines[1],
    paste0(indent_wrap, lines[-1], collapse = "  \n"),
    sep = "  \n"
  )
  result
}
