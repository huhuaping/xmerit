# Changelog

## xmerit (development version)

## xmerit 0.0.14

### New features

- **opt_line()**: New function to generate one or more lines of
  questionnaire option items in Quarto (.qmd). Supports checkbox style
  (HTML: `<input type="checkbox">`; Word: Unicode ballot box) and letter
  style (A. B. C.). Optional line wrapping by display width for
  A4-friendly layout. See vignette “opt_line(): 问卷题项选项行” and
  [`?opt_line`](https://huhuaping.github.io/xmerit/reference/opt_line.md).

- **Vignette**: New article “opt_line()” under Articles on the pkgdown
  site, with usage, styles (checkbox/letter), wrapping, and Quarto
  integration.

### Bug fixes

- **qx.eval()**: Fixed error when building the init-value table from
  `lm.val` (`rename_all` column count mismatch). Now constructs the
  two-column table with
  `tibble(vs = names(lm.val), value = unlist(lm.val, use.names = FALSE))`.

### Miscellaneous

- **R CMD check**: Replaced `@import stats` with
  `@importFrom stats lm formula model.frame terms` in `lx.est` to avoid
  conflict with dplyr’s `filter`/`lag`. Added `@importFrom stats pf` in
  `lm.pf`. Declared `start` and `end` in
  [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  for `qx.eval`, `qx.psm`, and `lx.psm` (column names in pipelines).

- **.Rbuildignore**: Added `^prompts$` so the top-level `prompts`
  directory is not shipped with the package.

- **pkgdown workflow**: CI now triggers only on branch `main` (removed
  `master`).

------------------------------------------------------------------------

## xmerit 0.0.13.9001

### New features

- Support PRV and SRV evaluation for PRF and SRF, compatible with Quarto
  equation.

### Bug fixes

- Fixed the wrong order of `lm` terms.

- Fixed the wrong Greek symbols and suffix number.
