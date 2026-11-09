# **Get ESPN NHL League Leaders**

**Get ESPN NHL League Leaders**

## Usage

``` r
espn_nhl_leaders(
  category = "offensive",
  season = most_recent_nhl_season(),
  season_type = 2L,
  limit = 50L,
  page = 1L,
  ...
)
```

## Arguments

- category:

  Stat category to display. For NHL one of `"offensive"`, `"defensive"`,
  `"general"`, `"penalties"`, or `NULL` for all categories. Defaults to
  `"offensive"`.

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).
  **ESPN's NHL `statistics/byathlete` endpoint currently ignores this
  parameter and always returns the current/most-recent season.** The
  argument is accepted for forward-compatibility but does not filter
  upstream. The returned `season` and `season_type` columns reflect the
  season ESPN actually served, not this value.

- season_type:

  Season type code (1 = pre, 2 = regular, 3 = post). Defaults to `2`.
  **ESPN's NHL `statistics/byathlete` endpoint currently ignores this
  parameter.** The returned `season_type` column reflects the type ESPN
  actually served.

- limit:

  Athletes per page (default `50`).

- page:

  Page number (default `1`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per athlete:

|  |  |  |
|----|----|----|
| col_name | types | description |
| season | integer | Season end-year ESPN actually served (from `currentSeason$year`). |
| season_type | integer | Season type id ESPN actually served (from `currentSeason$type$id`). |
| requested_season_year | integer | Season year from `requestedSeason` (mirrors `season`). |
| page | integer | Current page. |
| pagination_count | integer | Total athlete count. |
| pagination_limit | integer | Athletes per page. |
| pagination_pages | integer | Total page count. |
| league_id | character | ESPN league identifier. |
| league_name | character | League name. |
| league_abbreviation | character | League abbreviation. |
| athlete_id | character | ESPN athlete identifier. |
| athlete_display_name | character | Player display name. |
| athlete_short_name | character | Player short name. |
| athlete_slug | character | Player URL slug. |
| athlete_position | character | Position abbreviation. |
| athlete_jersey | character | Jersey number. |
| athlete_team_id | character | ESPN team identifier. |
| athlete_team_name | character | Team name. |
| athlete_team_short_name | character | Team short name. |
| athlete_team_logo | character | Team logo URL. |
| headshot_href | character | Player headshot URL. |
| debut_year | integer | Debut year. |
| age | integer | Player age. |
| offensive_goals | character | Goals (offensive category). |
| offensive_assists | character | Assists. |
| offensive_points | character | Points. |
| offensive_power_play_goals | character | Power-play goals. |
| offensive_shots_total | character | Shots on goal. |
| offensive_shooting_pct | character | Shooting percentage. |
| offensive_game_winning_goals | character | Game-winning goals. |
| offensive_rank_points | character | Rank by points. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_leaders(category = "offensive", limit = 25))
#> ── NHL Leaders data from ESPN.com ───────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:54 UTC
#> # A tibble: 25 × 51
#>    season season_type requested_season_year  page pagination_count
#>     <int>       <int>                 <int> <int>            <int>
#>  1   2027           2                  2027     1              833
#>  2   2027           2                  2027     1              833
#>  3   2027           2                  2027     1              833
#>  4   2027           2                  2027     1              833
#>  5   2027           2                  2027     1              833
#>  6   2027           2                  2027     1              833
#>  7   2027           2                  2027     1              833
#>  8   2027           2                  2027     1              833
#>  9   2027           2                  2027     1              833
#> 10   2027           2                  2027     1              833
#> # ℹ 15 more rows
#> # ℹ 46 more variables: pagination_limit <int>, pagination_pages <int>,
#> #   league_id <chr>, league_name <chr>, league_abbreviation <chr>,
#> #   athlete_id <chr>, athlete_display_name <chr>, athlete_short_name <chr>,
#> #   athlete_slug <chr>, athlete_position <chr>, athlete_jersey <chr>,
#> #   athlete_team_id <chr>, athlete_team_name <chr>,
#> #   athlete_team_short_name <chr>, athlete_team_logo <chr>, …
# }
```
