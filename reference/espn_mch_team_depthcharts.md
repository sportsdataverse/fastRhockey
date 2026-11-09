# **Get ESPN MCH / WCH Team Depth Charts**

**Get ESPN MCH / WCH Team Depth Charts**

## Usage

``` r
espn_mch_team_depthcharts(team_id, ...)

espn_wch_team_depthcharts(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"104"` for Chicago
  Blackhawks).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row containing team and season
metadata. NHL depth-slot player assignments are not served at this
endpoint; the response contains team/season context only:

|                  |           |                                         |
|------------------|-----------|-----------------------------------------|
| col_name         | types     | description                             |
| team_id          | character | ESPN team identifier (echoed from arg). |
| timestamp        | character | Response timestamp (ISO 8601).          |
| status           | character | Response status string.                 |
| season_year      | integer   | Season year.                            |
| season_type      | integer   | Season type code.                       |
| season_name      | character | Season type name.                       |
| team_espn_id     | character | ESPN team identifier.                   |
| abbreviation     | character | Team abbreviation.                      |
| location         | character | Team city/location.                     |
| name             | character | Team mascot name.                       |
| display_name     | character | Full display name.                      |
| color            | character | Primary color hex.                      |
| logo             | character | Team logo URL.                          |
| record_summary   | character | Record summary string.                  |
| season_summary   | character | Season summary string.                  |
| standing_summary | character | Standing summary string.                |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_team_depthcharts()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_depthcharts.md)
(columns inherited); a collection may be empty.

`espn_wch_team_depthcharts()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_team_depthcharts(team_id = "104"))
#> ── MENS-COLLEGE-HOCKEY Team Depth Charts data from ESPN.com ────────────────────
#> ℹ Data updated: 2026-11-09 17:18:51 UTC
#> # A tibble: 1 × 16
#>   team_id timestamp      status season_year season_type season_name team_espn_id
#>   <chr>   <chr>          <chr>        <int>       <int> <chr>       <chr>       
#> 1 104     2026-11-09T16… succe…        2026           3 Postseason  104         
#> # ℹ 9 more variables: abbreviation <chr>, location <chr>, name <chr>,
#> #   display_name <chr>, color <chr>, logo <chr>, record_summary <chr>,
#> #   season_summary <chr>, standing_summary <chr>
# }
# \donttest{
  try(espn_wch_team_depthcharts(team_id = "2137"))
#> ── WOMENS-COLLEGE-HOCKEY Team Depth Charts data from ESPN.com ──────────────────
#> ℹ Data updated: 2026-11-09 17:18:51 UTC
#> # A tibble: 1 × 16
#>   team_id timestamp      status season_year season_type season_name team_espn_id
#>   <chr>   <chr>          <chr>        <int>       <int> <chr>       <chr>       
#> 1 2137    2026-11-09T16… succe…        2027           2 Regular Se… 2137        
#> # ℹ 9 more variables: abbreviation <chr>, location <chr>, name <chr>,
#> #   display_name <chr>, color <chr>, logo <chr>, record_summary <chr>,
#> #   season_summary <chr>, standing_summary <chr>
# }
```
