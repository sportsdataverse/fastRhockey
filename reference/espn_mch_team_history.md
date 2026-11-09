# **Get ESPN MCH / WCH Team History**

**Get ESPN MCH / WCH Team History**

## Usage

``` r
espn_mch_team_history(team_id, ...)

espn_wch_team_history(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"104"` for Chicago
  Blackhawks).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per season entry when data is
available. The `/history` endpoint currently returns an empty response
for MCH; an empty tibble is returned with a cli warning. Columns when
populated include `team_id`, `season`, `wins`, `losses`, `ties`,
`ot_losses`, `points`, `summary`, and `description`.

Columns when populated:

|             |           |                                                  |
|-------------|-----------|--------------------------------------------------|
| col_name    | type      | description                                      |
| team_id     | character | ESPN team identifier (echoed from the argument). |
| season      | integer   | Season year.                                     |
| wins        | integer   | Wins.                                            |
| losses      | integer   | Losses.                                          |
| ties        | integer   | Ties.                                            |
| ot_losses   | integer   | Overtime losses.                                 |
| points      | integer   | Points.                                          |
| summary     | character | Record summary string.                           |
| description | character | Season description.                              |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_team_history()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_history.md)
(columns inherited); a collection may be empty.

`espn_wch_team_history()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_team_history(team_id = "104"))
#> ! ESPN MENS-COLLEGE-HOCKEY team history returned no data for team_id=104. This endpoint may not be populated for MENS-COLLEGE-HOCKEY.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_team_history(team_id = "2137"))
#> ! ESPN WOMENS-COLLEGE-HOCKEY team history returned no data for team_id=2137. This endpoint may not be populated for WOMENS-COLLEGE-HOCKEY.
#> data frame with 0 columns and 0 rows
# }
```
