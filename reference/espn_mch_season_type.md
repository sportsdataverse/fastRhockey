# **Get ESPN MCH / WCH Season Type (core-v2)**

**Get ESPN MCH / WCH Season Type (core-v2)**

## Usage

``` r
espn_mch_season_type(season = most_recent_nhl_season(), season_type = 2, ...)

espn_wch_season_type(season = most_recent_nhl_season(), season_type = 2, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- season_type:

  Season type code: `1`=pre-season, `2`=regular season, `3`=post-season,
  `4`=off-season. Defaults to `2`.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with season-type metadata:

|                 |           |                                           |
|-----------------|-----------|-------------------------------------------|
| col_name        | types     | description                               |
| season          | integer   | Season year (echoed from arg).            |
| season_type     | integer   | Season type code (echoed from arg).       |
| id              | character | Season type identifier.                   |
| type            | integer   | Season type numeric code.                 |
| name            | character | Season type name (e.g. "Regular Season"). |
| abbreviation    | character | Season type abbreviation (e.g. "reg").    |
| slug            | character | Season type slug (e.g. "regular-season"). |
| year            | integer   | Season year from the API.                 |
| start_date      | character | Season type start date (ISO 8601).        |
| end_date        | character | Season type end date (ISO 8601).          |
| has_groups      | logical   | Whether this season type has groups.      |
| has_standings   | logical   | Whether this season type has standings.   |
| has_legs        | logical   | Whether this season type has legs.        |
| groups_ref      | character | `$ref` URL for the groups collection.     |
| weeks_ref       | character | `$ref` URL for the weeks collection.      |
| corrections_ref | character | `$ref` URL for corrections.               |
| leaders_ref     | character | `$ref` URL for leaders.                   |
| type_ref        | character | `$ref` URL for this season-type object.   |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_type()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_type.md)
(columns inherited); a collection may be empty.

`espn_wch_season_type()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_season_type(season = 2026, season_type = 2))
#> ── MENS-COLLEGE-HOCKEY Season Type data from ESPN core-v2 ──────────────────────
#> ℹ Data updated: 2026-11-09 11:49:31 UTC
#> # A tibble: 1 × 18
#>   season season_type id     type name        abbreviation slug   year start_date
#>    <int>       <int> <chr> <int> <chr>       <chr>        <chr> <int> <chr>     
#> 1   2026           2 2         2 Regular Se… reg          regu…  2026 2025-09-0…
#> # ℹ 9 more variables: end_date <chr>, has_groups <lgl>, has_standings <lgl>,
#> #   has_legs <lgl>, groups_ref <chr>, weeks_ref <chr>, corrections_ref <chr>,
#> #   leaders_ref <chr>, type_ref <chr>
# }
# \donttest{
  try(espn_wch_season_type(season = 2026, season_type = 2))
#> ── WOMENS-COLLEGE-HOCKEY Season Type data from ESPN core-v2 ────────────────────
#> ℹ Data updated: 2026-11-09 11:49:31 UTC
#> # A tibble: 1 × 18
#>   season season_type id     type name        abbreviation slug   year start_date
#>    <int>       <int> <chr> <int> <chr>       <chr>        <chr> <int> <chr>     
#> 1   2026           2 2         2 Regular Se… reg          regu…  2026 2025-09-0…
#> # ℹ 9 more variables: end_date <chr>, has_groups <lgl>, has_standings <lgl>,
#> #   has_legs <lgl>, groups_ref <chr>, weeks_ref <chr>, corrections_ref <chr>,
#> #   leaders_ref <chr>, type_ref <chr>
# }
```
