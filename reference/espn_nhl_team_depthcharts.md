# **Get ESPN NHL Team Depth Charts**

**Get ESPN NHL Team Depth Charts**

## Usage

``` r
espn_nhl_team_depthcharts(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"4"` for Chicago
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_team_depthcharts(team_id = "4"))
#> ── NHL Team Depth Charts data from ESPN.com ─────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:27:50 UTC
#> # A tibble: 1 × 16
#>   team_id timestamp      status season_year season_type season_name team_espn_id
#>   <chr>   <chr>          <chr>        <int>       <int> <chr>       <chr>       
#> 1 4       2026-11-13T21… succe…        2027           2 Regular Se… 4           
#> # ℹ 9 more variables: abbreviation <chr>, location <chr>, name <chr>,
#> #   display_name <chr>, color <chr>, logo <chr>, record_summary <chr>,
#> #   season_summary <chr>, standing_summary <chr>
# }
```
