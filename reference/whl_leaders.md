# **WHL Statistical Leaders**

WHL statistical leaders for a season from the HockeyTech feed.

## Usage

``` r
whl_leaders(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional (defaults to most-recent).

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per player entry.

## See also

Other WHL Functions:
[`most_recent_whl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_whl_season.md),
[`whl`](https://fastRhockey.sportsdataverse.org/reference/whl.md),
[`whl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_corsi.md),
[`whl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_shifts.md),
[`whl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_summary.md),
[`whl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/whl_pbp.md),
[`whl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_stats.md),
[`whl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_toi.md),
[`whl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/whl_schedule.md),
[`whl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/whl_season_id.md),
[`whl_standings()`](https://fastRhockey.sportsdataverse.org/reference/whl_standings.md),
[`whl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/whl_team_roster.md),
[`whl_teams()`](https://fastRhockey.sportsdataverse.org/reference/whl_teams.md)

## Examples

``` r
 try(whl_leaders()) 
#> ── WHL Leaders from HockeyTech ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-30 14:46:13 UTC
#> # A tibble: 10 × 15
#>     rank player_id jersey_number name      team_id team_name team_code team_logo
#>    <int> <chr>     <chr>         <chr>     <chr>   <chr>     <chr>     <chr>    
#>  1     1 29105     27            Hunter L… 213     Saskatoo… SAS       https://…
#>  2     2 29162     4             Brayden … 213     Saskatoo… SAS       https://…
#>  3     3 29428     16            Cooper W… 213     Saskatoo… SAS       https://…
#>  4     4 29669     20            Ben Harv… 209     Prince A… PA        https://…
#>  5     5 30623     11            Gavin Ka… 277     Penticto… PEN       https://…
#>  6     1 29105     27            Hunter L… 213     Saskatoo… SAS       https://…
#>  7     2 29516     21            Beckett … 211     Red Deer… RD        https://…
#>  8     3 29670     22            Connor H… 209     Prince A… PA        https://…
#>  9     4 29162     4             Brayden … 213     Saskatoo… SAS       https://…
#> 10     5 29669     20            Ben Harv… 209     Prince A… PA        https://…
#> # ℹ 7 more variables: team_logo_small <chr>, stat_formatted <chr>,
#> #   type_formatted <chr>, photo <chr>, photo_small <chr>, position <chr>,
#> #   division <chr>
```
