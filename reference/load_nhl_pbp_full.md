# **Load fastRhockey NHL play-by-play (full, alias)**

Alias of
[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md)
for naming parity with sportsdataverse-py. In this package
[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md)
already serves the full `nhl_pbp_full` release.

## Usage

``` r
load_nhl_pbp_full(seasons = most_recent_nhl_season(), ...)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2011.

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database (used by
  [`update_nhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_nhl_db.md)).

## Value

See
[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md).

## See also

[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md)
[`load_nhl_pbp_lite()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp_lite.md)

Other NHL Loader Functions:
[`load_nhl_goalie_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_goalie_boxscores.md),
[`load_nhl_player_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_boxscore.md),
[`load_nhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_boxscores.md),
[`load_nhl_schedules()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedules.md),
[`load_nhl_skater_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_skater_boxscores.md),
[`load_nhl_team_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_boxscore.md),
[`load_nhl_team_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_boxscores.md)

## Examples

``` r
# \donttest{
  try(load_nhl_pbp_full(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 1,106,678 × 94
#>    event_type   event secondary_type event_team_abbr event_team_type description
#>    <chr>        <chr> <chr>          <chr>           <chr>           <chr>      
#>  1 CHANGE       Chan… NA             NA              NA              ON: Ryan M…
#>  2 CHANGE       Chan… NA             NA              NA              ON: Victor…
#>  3 FACEOFF      Face… NA             NSH             away            Ryan O'Rei…
#>  4 PERIOD_START Peri… NA             NA              NA              Start of P…
#>  5 CHANGE       Chan… NA             NA              NA              ON: Roman …
#>  6 CHANGE       Chan… NA             NA              NA              ON: Luke S…
#>  7 CHANGE       Chan… NA             NA              NA              ON: Yakov …
#>  8 CHANGE       Chan… NA             NA              NA              ON: Nikita…
#>  9 CHANGE       Chan… NA             NA              NA              ON: Steven…
#> 10 STOP         Stop… NA             NA              NA              Stoppage i…
#> # ℹ 1,106,668 more rows
#> # ℹ 88 more variables: period <int>, period_type <chr>, period_time <chr>,
#> #   period_seconds <int>, period_seconds_remaining <int>,
#> #   period_time_remaining <chr>, game_seconds <int>,
#> #   game_seconds_remaining <int>, home_score <int>, away_score <int>,
#> #   event_player_1_name <chr>, event_player_1_type <chr>,
#> #   event_player_1_id <int>, event_player_2_name <chr>, …
# }
```
