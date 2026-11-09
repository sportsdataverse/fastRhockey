# **Get ESPN NHL Season Info (core-v2)**

**Get ESPN NHL Season Info (core-v2)**

## Usage

``` r
espn_nhl_season_info(season = most_recent_nhl_season(), ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with season metadata:

|              |           |                                             |
|--------------|-----------|---------------------------------------------|
| col_name     | types     | description                                 |
| season       | integer   | Season year (echoed from arg).              |
| start_date   | character | Season start date (ISO 8601).               |
| end_date     | character | Season end date (ISO 8601).                 |
| display_name | character | Season display name (e.g. "2025-26").       |
| season_ref   | character | `$ref` URL for this season.                 |
| types_ref    | character | `$ref` URL for the season-types collection. |
| type_ref     | character | `$ref` URL for the current season type.     |
| rankings_ref | character | `$ref` URL for rankings.                    |
| coaches_ref  | character | `$ref` URL for coaches.                     |
| athletes_ref | character | `$ref` URL for athletes.                    |
| awards_ref   | character | `$ref` URL for awards.                      |
| futures_ref  | character | `$ref` URL for futures.                     |
| leaders_ref  | character | `$ref` URL for leaders.                     |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_season_info(season = 2026))
#> ── NHL Season Info data from ESPN core-v2 ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:51:32 UTC
#> # A tibble: 1 × 13
#>   season start_date        end_date   display_name season_ref types_ref type_ref
#>    <int> <chr>             <chr>      <chr>        <chr>      <chr>     <chr>   
#> 1   2026 2025-09-20T07:00Z 2026-07-0… 2025-26      http://sp… http://s… http://…
#> # ℹ 6 more variables: rankings_ref <chr>, coaches_ref <chr>,
#> #   athletes_ref <chr>, awards_ref <chr>, futures_ref <chr>, leaders_ref <chr>
# }
```
