# Translate text from English to Chinese along data frame

Translate text from English to Chinese along data frame

## Usage

``` r
gg_translate(df, col_key, col_trans, ggkey)
```

## Arguments

- df:

  data frame. The names of columns should be non-multibytes string.

- col_key:

  character. Which column will be kept as a "key column", and can be
  used with data frame join (`left_join()`).

- col_trans:

  character. Which column will be translated.

- ggkey:

  character. google translate API key, which is a long string containing
  upper and lower case letters, numbers, and dashes, such as
  `a4db08b7-5729-4ba9-8c08-f2df493465a1`.

## Value

result

## Examples

``` r
df_my <- tibble::tibble(
  index = 1:3,
  target = c(
    'We recommend you use the Cloud Console to manage API keys.',
    'Your API keys are shown in the API keys section.',
    'Pass the API key into a REST API call as a query parameter with the following format.'
    )
 )
if (FALSE) { # \dontrun{
result_trans <- gg_translate(
  df = df_my,
  col_key = "index",
  col_trans = "target",
  ggkey = keyring::key_get("key",keyring = "gg-trans"))
} # }
```
