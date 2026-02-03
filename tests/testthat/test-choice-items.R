# test-choice-items.R
# Tests for opt_line() and internal helpers (via opt_line behavior).

test_that("opt_line returns character", {
  expect_type(opt_line(c("A", "B")), "character")
  expect_length(opt_line(c("A", "B")), 1L)
})

test_that("opt_line handles empty or invalid input", {
  expect_identical(opt_line(character(0)), "")
  expect_identical(opt_line(c()), "")
})

test_that("opt_line letter style produces A. B. pattern", {
  out <- opt_line(c("Yes", "No"), style = "letter")
  expect_match(out, "A\\.")
  expect_match(out, "B\\.")
  expect_match(out, "Yes")
  expect_match(out, "No")
})

test_that("opt_line letter_sep is applied when style is letter", {
  out <- opt_line(c("X", "Y"), style = "letter", letter_sep = ", ")
  expect_match(out, "A, ")
  expect_match(out, "B, ")
})

test_that("opt_line wrap=TRUE returns single string", {
  out <- opt_line(
    c("Short", "Also short", "Much longer option text here"),
    wrap = TRUE,
    max_width = 20L
  )
  expect_type(out, "character")
  expect_length(out, 1L)
})

test_that("opt_line with one option", {
  out <- opt_line("Only")
  expect_length(out, 1L)
  expect_true(nchar(out) >= 4L)
})

test_that("idx_to_letter behavior via opt_line letter style", {
  out <- opt_line(LETTERS[1:3], style = "letter")
  expect_match(out, "A\\.")
  expect_match(out, "B\\.")
  expect_match(out, "C\\.")
})
