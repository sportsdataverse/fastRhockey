# **Get ESPN MCH / WCH Transactions**

**Get ESPN MCH / WCH Transactions**

## Usage

``` r
espn_mch_transactions(limit = 500, ...)

espn_wch_transactions(limit = 500, ...)
```

## Arguments

- limit:

  Maximum number of transactions (ESPN `limit`; default `500`, as
  sportsdataverse-py).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per transaction:

|                   |           |                              |
|-------------------|-----------|------------------------------|
| col_name          | types     | description                  |
| date              | character | Transaction date (ISO 8601). |
| description       | character | Transaction description.     |
| team_id           | character | ESPN team identifier.        |
| team_location     | character | Team city/location.          |
| team_name         | character | Team mascot/name.            |
| team_abbreviation | character | Team abbreviation.           |
| team_display_name | character | Team display name.           |
| team_color        | character | Team primary color hex.      |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_transactions()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_transactions.md);
ESPN answered an empty collection for every captured MCH body (2026-10),
so the columns below are the NHL helper's and unverified for this
league.

`espn_wch_transactions()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_transactions())
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_transactions())
#> data frame with 0 columns and 0 rows
# }
```
