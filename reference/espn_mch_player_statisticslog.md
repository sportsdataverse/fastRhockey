# **Get ESPN MCH / WCH Player Statistics Log (core-v2)**

**Get ESPN MCH / WCH Player Statistics Log (core-v2)**

## Usage

``` r
espn_mch_player_statisticslog(athlete_id, ...)

espn_wch_player_statisticslog(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per season-entry. This endpoint
is the MCH game-log substitute (web-v3 gamelog 404s for MCH). Each row
carries `$ref` URLs to follow for per-season totals and team splits.

|                 |           |                                            |
|-----------------|-----------|--------------------------------------------|
| col_name        | types     | description                                |
| athlete_id      | character | ESPN athlete id (echoed from arg).         |
| season          | integer   | Season year parsed from the season `$ref`. |
| season_ref      | character | `$ref` URL for the season object.          |
| total_stats_ref | character | `$ref` URL for season totals statistics.   |
| team_stats_ref  | character | `$ref` URL for team-level statistics.      |
| team_ref        | character | `$ref` URL for the team object.            |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_statisticslog()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_statisticslog.md)
(columns inherited); a collection may be empty.

`espn_wch_player_statisticslog()` is the same wrapper for women's
college hockey (league slug `womens-college-hockey`): same arguments,
same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_player_statisticslog(athlete_id = "5136656"))
#> ── MENS-COLLEGE-HOCKEY Player Statisticslog data from ESPN core-v2 ─────────────
#> ℹ Data updated: 2026-11-09 22:50:36 UTC
#> # A tibble: 2 × 6
#>   athlete_id season season_ref           total_stats_ref team_stats_ref team_ref
#>   <chr>       <int> <chr>                <chr>           <chr>          <chr>   
#> 1 5136656      2025 http://sports.core.… NA              http://sports… http://…
#> 2 5136656      2024 http://sports.core.… NA              http://sports… http://…
# }
# \donttest{
  try(espn_wch_player_statisticslog(athlete_id = "5263060"))
#> ✖ 2026-11-09 22:50:36.623311: Invalid arguments or no ESPN womens-college-hockey player statisticslog for athlete 5263060 available!
#> ✖ Args: league = "womens-college-hockey", athlete_id = "5263060"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
