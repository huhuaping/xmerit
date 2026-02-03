# Generate one or more lines of questionnaire option items

Produces a single line (or wrapped lines) of options for questionnaire
items in Quarto qmd, supporting HTML and Word (docx). Format is chosen
automatically from the current Quarto output.

## Usage

``` r
opt_line(
  options,
  gap_n = 8L,
  style = NULL,
  letter_sep = ". ",
  wrap = FALSE,
  max_width = 42L,
  indent_wrap = "   "
)
```

## Arguments

- options:

  Character vector of option labels (e.g. `c("Yes", "No")`).

- gap_n:

  Number of gap units between options (default 8). HTML uses `&nbsp;`,
  Word uses Unicode no-break space.

- style:

  `"checkbox"` or `"letter"`. If `NULL`, uses
  `getOption("questionnaire.opt_style", "checkbox")` so you can set
  `options(questionnaire.opt_style = "letter")` in qmd setup.

- letter_sep:

  Separator between letter and text when `style = "letter"` (e.g. `". "`
  or `", "`).

- wrap:

  If `TRUE`, wrap by `max_width`; use with `indent_wrap`.

- max_width:

  Maximum display width per line in half-width units (default 42).

- indent_wrap:

  Indent string for continuation lines (default 3 spaces).

## Value

Length-1 character vector (raw HTML when checkbox + HTML, via
[`knitr::asis_output`](https://rdrr.io/pkg/knitr/man/asis_output.html)).
Use in qmd inline R, e.g. `opt_line(...)`.

## Styles

- **Checkbox** (`style = "checkbox"`): In HTML, outputs
  `<input type="checkbox">`; in Word, outputs Unicode ballot box
  (U+2610) for print-and-fill.

- **Letter** (`style = "letter"`): Labels like `A.`, `B.`, `C.`.

## Line wrapping

When `wrap = TRUE`, options are wrapped by display width (`max_width` in
half-width units). CJK characters count as 2 units, ASCII as 1, for
A4-friendly layout.

## Examples

``` r
# Checkbox style (default)
opt_line(c("Male", "Female"))
#> [1] "☐ Male        ☐ Female"
opt_line(c("Very much", "Somewhat", "Neutral", "A little", "Not at all"))
#> [1] "☐ Very much        ☐ Somewhat        ☐ Neutral        ☐ A little        ☐ Not at all"

# Line wrapping for long options (A4)
opt_line(c("Too expensive", "No need", "Good health"),
         wrap = TRUE, max_width = 42)
#> [1] "☐ Too expensive        ☐ No need  \n   ☐ Good health"

# Letter style
opt_line(c("Yes", "No"), style = "letter")
#> [1] "A. Yes        B. No"
opt_line(c("Yes", "No"), style = "letter", letter_sep = ", ")
#> [1] "A, Yes        B, No"
```
