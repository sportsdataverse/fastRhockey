# **Get ESPN MCH / WCH Season Players (core-v2)**

**Get ESPN MCH / WCH Season Players (core-v2)**

## Usage

``` r
espn_mch_season_players(
  season = most_recent_nhl_season(),
  limit = 100,
  page = 1,
  ...
)

espn_wch_season_players(
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

  Number of players per page (default `100`). The MCH season player list
  exceeds 11,000 entries across ~2,300 pages; use `page` to iterate.

- page:

  Page number (1-indexed, default `1`). Check the returned `page_count`
  column to determine how many pages are available.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per player reference for the
requested page. This function hits the same endpoint as
`espn_mch_season_athletes()` (`seasons/{season}/athletes`) but uses the
`player_id` column name for consistency with the `sportsdataverse-py`
`espn_mch_season_players` naming convention:

|            |           |                                              |
|------------|-----------|----------------------------------------------|
| col_name   | types     | description                                  |
| ref        | character | `$ref` URL for the athlete-in-season object. |
| player_id  | character | ESPN player id parsed from the `$ref` URL.   |
| season     | integer   | Season year (echoed from arg).               |
| page       | integer   | Page number (echoed from arg).               |
| count      | integer   | Total players in the collection.             |
| page_count | integer   | Total pages in the collection.               |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_players()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_players.md)
(columns inherited); a collection may be empty.

`espn_wch_season_players()` is the same wrapper for women's college
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
  try(espn_mch_season_players(season = 2026, limit = 10, page = 1))
#> ── MENS-COLLEGE-HOCKEY Season Players data from ESPN core-v2 ───────────────────
#> ℹ Data updated: 2026-11-09 22:50:43 UTC
#> # A tibble: 10 × 6
#>    ref                                   player_id season  page count page_count
#>    <chr>                                 <chr>      <int> <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/s… 2029152     2026     1  2680        268
#>  2 http://sports.core.api.espn.com/v2/s… 2127132     2026     1  2680        268
#>  3 http://sports.core.api.espn.com/v2/s… 2127153     2026     1  2680        268
#>  4 http://sports.core.api.espn.com/v2/s… 2178672     2026     1  2680        268
#>  5 http://sports.core.api.espn.com/v2/s… 2200605     2026     1  2680        268
#>  6 http://sports.core.api.espn.com/v2/s… 2200887     2026     1  2680        268
#>  7 http://sports.core.api.espn.com/v2/s… 2200989     2026     1  2680        268
#>  8 http://sports.core.api.espn.com/v2/s… 2208561     2026     1  2680        268
#>  9 http://sports.core.api.espn.com/v2/s… 2229972     2026     1  2680        268
#> 10 http://sports.core.api.espn.com/v2/s… 2263465     2026     1  2680        268
# }
# \donttest{
  try(espn_wch_season_players(season = 2026, limit = 10, page = 1))
#> ── WOMENS-COLLEGE-HOCKEY Season Players data from ESPN core-v2 ─────────────────
#> ℹ Data updated: 2026-11-09 22:50:43 UTC
#> # A tibble: 10 × 6
#>    ref                                   player_id season  page count page_count
#>    <chr>                                 <chr>      <int> <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/s… 3988803     2026     1   727         73
#>  2 http://sports.core.api.espn.com/v2/s… 4197165     2026     1   727         73
#>  3 http://sports.core.api.espn.com/v2/s… 4419646     2026     1   727         73
#>  4 http://sports.core.api.espn.com/v2/s… 4565255     2026     1   727         73
#>  5 http://sports.core.api.espn.com/v2/s… 4565270     2026     1   727         73
#>  6 http://sports.core.api.espn.com/v2/s… 4587583     2026     1   727         73
#>  7 http://sports.core.api.espn.com/v2/s… 4588743     2026     1   727         73
#>  8 http://sports.core.api.espn.com/v2/s… 4781545     2026     1   727         73
#>  9 http://sports.core.api.espn.com/v2/s… 4913605     2026     1   727         73
#> 10 http://sports.core.api.espn.com/v2/s… 4913613     2026     1   727         73
# }
```
