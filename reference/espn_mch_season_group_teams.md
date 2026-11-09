# **Get ESPN MCH / WCH Season Group Teams (core-v2)**

**Get ESPN MCH / WCH Season Group Teams (core-v2)**

## Usage

``` r
espn_mch_season_group_teams(
  season = most_recent_nhl_season(),
  season_type = 2,
  group_id,
  limit = 500,
  ...
)

espn_wch_season_group_teams(
  season = most_recent_nhl_season(),
  season_type = 2,
  group_id,
  limit = 500,
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
  [`espn_mch_season_groups()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_groups.md)
  to discover valid ids for a season/type.

- limit:

  Maximum number of teams to return (default `500`).

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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_group_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_group_teams.md)
(columns inherited); a collection may be empty.

`espn_wch_season_group_teams()` is the same wrapper for women's college
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
  try({
    grps <- espn_mch_season_groups(season = 2026, season_type = 2)
    espn_mch_season_group_teams(season = 2026, season_type = 2,
                                 group_id = grps$group_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Season Group Teams data from ESPN core-v2 ───────────────
#> ℹ Data updated: 2026-11-09 19:34:04 UTC
#> # A tibble: 11 × 5
#>    ref                                       team_id season season_type group_id
#>    <chr>                                     <chr>    <int>       <int>    <int>
#>  1 http://sports.core.api.espn.com/v2/sport… 107       2026           2       61
#>  2 http://sports.core.api.espn.com/v2/sport… 178       2026           2       61
#>  3 http://sports.core.api.espn.com/v2/sport… 315       2026           2       61
#>  4 http://sports.core.api.espn.com/v2/sport… 349       2026           2       61
#>  5 http://sports.core.api.espn.com/v2/sport… 2005      2026           2       61
#>  6 http://sports.core.api.espn.com/v2/sport… 2022      2026           2       61
#>  7 http://sports.core.api.espn.com/v2/sport… 2060      2026           2       61
#>  8 http://sports.core.api.espn.com/v2/sport… 2099      2026           2       61
#>  9 http://sports.core.api.espn.com/v2/sport… 2385      2026           2       61
#> 10 http://sports.core.api.espn.com/v2/sport… 2523      2026           2       61
#> 11 http://sports.core.api.espn.com/v2/sport… 2529      2026           2       61
# }
# \donttest{
  try({
    grps <- espn_wch_season_groups(season = 2026, season_type = 2)
    espn_wch_season_group_teams(season = 2026, season_type = 2,
                                 group_id = grps$group_id[1])
  })
#> ✖ 2026-11-09 19:34:04.804518: Invalid arguments or no ESPN womens-college-hockey season groups for 2026 type 2 available!
#> ✖ Args: league = "womens-college-hockey", season = 2026, season_type = 2
#> ✖ Error: replacement has 1 row, data has 0
#> ✖ 2026-11-09 19:34:04.82795: 
#> ✖ Args: league = "womens-college-hockey", season = 2026, season_type = 2, group_id = NULL, limit = 500
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
