# **Get ESPN NHL Season Group Teams (core-v2)**

**Get ESPN NHL Season Group Teams (core-v2)**

## Usage

``` r
espn_nhl_season_group_teams(
  season = most_recent_nhl_season(),
  season_type = 2,
  group_id,
  limit = 100,
  ...
)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- season_type:

  Season type code: `1`=pre-season, `2`=regular season, `3`=post-season,
  `4`=off-season. Defaults to `2`.

- group_id:

  ESPN group identifier (integer or character). Use
  [`espn_nhl_season_groups()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_groups.md)
  to discover valid ids for a season/type.

- limit:

  Maximum number of teams to return (default `100`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per team:

|             |           |                                          |
|-------------|-----------|------------------------------------------|
| col_name    | types     | description                              |
| ref         | character | `$ref` URL for the team-season object.   |
| team_id     | character | ESPN team id parsed from the `$ref` URL. |
| season      | integer   | Season year (echoed from arg).           |
| season_type | integer   | Season type code (echoed from arg).      |
| group_id    | integer   | Group id (echoed from arg).              |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    grps <- espn_nhl_season_groups(season = 2026, season_type = 2)
    espn_nhl_season_group_teams(season = 2026, season_type = 2,
                                 group_id = grps$group_id[1])
  })
#> ── NHL Season Group Teams data from ESPN core-v2 ────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:50:05 UTC
#> # A tibble: 16 × 5
#>    ref                                       team_id season season_type group_id
#>    <chr>                                     <chr>    <int>       <int>    <int>
#>  1 http://sports.core.api.espn.com/v2/sport… 1         2026           2        7
#>  2 http://sports.core.api.espn.com/v2/sport… 2         2026           2        7
#>  3 http://sports.core.api.espn.com/v2/sport… 5         2026           2        7
#>  4 http://sports.core.api.espn.com/v2/sport… 7         2026           2        7
#>  5 http://sports.core.api.espn.com/v2/sport… 10        2026           2        7
#>  6 http://sports.core.api.espn.com/v2/sport… 11        2026           2        7
#>  7 http://sports.core.api.espn.com/v2/sport… 12        2026           2        7
#>  8 http://sports.core.api.espn.com/v2/sport… 13        2026           2        7
#>  9 http://sports.core.api.espn.com/v2/sport… 14        2026           2        7
#> 10 http://sports.core.api.espn.com/v2/sport… 15        2026           2        7
#> 11 http://sports.core.api.espn.com/v2/sport… 16        2026           2        7
#> 12 http://sports.core.api.espn.com/v2/sport… 20        2026           2        7
#> 13 http://sports.core.api.espn.com/v2/sport… 21        2026           2        7
#> 14 http://sports.core.api.espn.com/v2/sport… 23        2026           2        7
#> 15 http://sports.core.api.espn.com/v2/sport… 26        2026           2        7
#> 16 http://sports.core.api.espn.com/v2/sport… 29        2026           2        7
# }
```
