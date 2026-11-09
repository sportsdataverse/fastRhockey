# **Get ESPN NHL Season Players (core-v2)**

**Get ESPN NHL Season Players (core-v2)**

## Usage

``` r
espn_nhl_season_players(
  season = most_recent_nhl_season(),
  limit = 100,
  page = 1,
  ...
)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- limit:

  Number of players per page (default `100`). The NHL season player list
  exceeds 11,000 entries across ~2,300 pages; use `page` to iterate.

- page:

  Page number (1-indexed, default `1`). Check the returned `page_count`
  column to determine how many pages are available.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per player reference for the
requested page. This function hits the same endpoint as
[`espn_nhl_season_athletes()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_athletes.md)
(`seasons/{season}/athletes`) but uses the `player_id` column name for
consistency with the `sportsdataverse-py` `espn_nhl_season_players`
naming convention:

|            |           |                                              |
|------------|-----------|----------------------------------------------|
| col_name   | types     | description                                  |
| ref        | character | `$ref` URL for the athlete-in-season object. |
| player_id  | character | ESPN player id parsed from the `$ref` URL.   |
| season     | integer   | Season year (echoed from arg).               |
| page       | integer   | Page number (echoed from arg).               |
| count      | integer   | Total players in the collection.             |
| page_count | integer   | Total pages in the collection.               |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_season_players(season = 2026, limit = 10, page = 1))
#> ── NHL Season Players data from ESPN core-v2 ────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:57 UTC
#> # A tibble: 10 × 6
#>    ref                                   player_id season  page count page_count
#>    <chr>                                 <chr>      <int> <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/s… 1           2026     1 11976       1198
#>  2 http://sports.core.api.espn.com/v2/s… 2           2026     1 11976       1198
#>  3 http://sports.core.api.espn.com/v2/s… 3           2026     1 11976       1198
#>  4 http://sports.core.api.espn.com/v2/s… 4           2026     1 11976       1198
#>  5 http://sports.core.api.espn.com/v2/s… 5           2026     1 11976       1198
#>  6 http://sports.core.api.espn.com/v2/s… 6           2026     1 11976       1198
#>  7 http://sports.core.api.espn.com/v2/s… 7           2026     1 11976       1198
#>  8 http://sports.core.api.espn.com/v2/s… 8           2026     1 11976       1198
#>  9 http://sports.core.api.espn.com/v2/s… 9           2026     1 11976       1198
#> 10 http://sports.core.api.espn.com/v2/s… 10          2026     1 11976       1198
# }
```
