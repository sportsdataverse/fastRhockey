# **Get ESPN NHL Player Statistics Log (core-v2)**

**Get ESPN NHL Player Statistics Log (core-v2)**

## Usage

``` r
espn_nhl_player_statisticslog(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per season-entry. This endpoint
is the NHL game-log substitute (web-v3 gamelog 404s for NHL). Each row
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_statisticslog(athlete_id = "4024820"))
#> ── NHL Player Statisticslog data from ESPN core-v2 ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:51:26 UTC
#> # A tibble: 10 × 6
#>    athlete_id season season_ref          total_stats_ref team_stats_ref team_ref
#>    <chr>       <int> <chr>               <chr>           <chr>          <chr>   
#>  1 4024820      2026 http://sports.core… http://sports.… http://sports… http://…
#>  2 4024820      2025 http://sports.core… http://sports.… http://sports… http://…
#>  3 4024820      2024 http://sports.core… http://sports.… http://sports… http://…
#>  4 4024820      2023 http://sports.core… http://sports.… http://sports… http://…
#>  5 4024820      2022 http://sports.core… http://sports.… http://sports… http://…
#>  6 4024820      2021 http://sports.core… http://sports.… http://sports… http://…
#>  7 4024820      2020 http://sports.core… http://sports.… http://sports… http://…
#>  8 4024820      2019 http://sports.core… http://sports.… http://sports… http://…
#>  9 4024820      2018 http://sports.core… http://sports.… http://sports… http://…
#> 10 4024820      2017 http://sports.core… http://sports.… http://sports… http://…
# }
```
