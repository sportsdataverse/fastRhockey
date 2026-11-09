# **Get ESPN NHL Draft**

**Get ESPN NHL Draft**

## Usage

``` r
espn_nhl_draft(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with draft data, or an empty tibble when the
endpoint is unavailable. **Note**: The NHL draft endpoint
(`site/v2/sports/hockey/nhl/draft`) currently returns HTTP 500 for NHL -
an empty tibble is returned gracefully:

|          |           |                                       |
|----------|-----------|---------------------------------------|
| col_name | types     | description                           |
| id       | character | Draft entry identifier (if returned). |
| name     | character | Draft entry name (if returned).       |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for NHL (ESPN returns HTTP 500 for this endpoint)
  try(espn_nhl_draft())
#> data frame with 0 columns and 0 rows
# }
```
