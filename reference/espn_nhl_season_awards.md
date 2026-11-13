# **Get ESPN NHL Season Awards (core-v2)**

**Get ESPN NHL Season Awards (core-v2)**

## Usage

``` r
espn_nhl_season_awards(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per award reference, or an
empty tibble when no awards are available for the season. Awards are
typically populated after the regular season ends:

|            |           |                                           |
|------------|-----------|-------------------------------------------|
| col_name   | types     | description                               |
| ref        | character | `$ref` URL for the award object.          |
| award_id   | character | ESPN award id parsed from the `$ref` URL. |
| season     | integer   | Season year (echoed from arg).            |
| count      | integer   | Total awards in the collection.           |
| page_count | integer   | Total pages in the collection.            |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_season_awards(season = 2025))
#> ── NHL Season Awards data from ESPN core-v2 ─────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:27:19 UTC
#> # A tibble: 17 × 5
#>    ref                                          award_id season count page_count
#>    <chr>                                        <chr>     <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/h… 498        2025    17          1
#>  2 http://sports.core.api.espn.com/v2/sports/h… 497        2025    17          1
#>  3 http://sports.core.api.espn.com/v2/sports/h… 499        2025    17          1
#>  4 http://sports.core.api.espn.com/v2/sports/h… 101        2025    17          1
#>  5 http://sports.core.api.espn.com/v2/sports/h… 100        2025    17          1
#>  6 http://sports.core.api.espn.com/v2/sports/h… 110        2025    17          1
#>  7 http://sports.core.api.espn.com/v2/sports/h… 106        2025    17          1
#>  8 http://sports.core.api.espn.com/v2/sports/h… 112        2025    17          1
#>  9 http://sports.core.api.espn.com/v2/sports/h… 111        2025    17          1
#> 10 http://sports.core.api.espn.com/v2/sports/h… 107        2025    17          1
#> 11 http://sports.core.api.espn.com/v2/sports/h… 103        2025    17          1
#> 12 http://sports.core.api.espn.com/v2/sports/h… 98         2025    17          1
#> 13 http://sports.core.api.espn.com/v2/sports/h… 97         2025    17          1
#> 14 http://sports.core.api.espn.com/v2/sports/h… 109        2025    17          1
#> 15 http://sports.core.api.espn.com/v2/sports/h… 197        2025    17          1
#> 16 http://sports.core.api.espn.com/v2/sports/h… 108        2025    17          1
#> 17 http://sports.core.api.espn.com/v2/sports/h… 105        2025    17          1
# }
```
