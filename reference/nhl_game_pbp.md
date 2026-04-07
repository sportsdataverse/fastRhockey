# **NHL Game Play-by-Play**

Returns detailed play-by-play data for a given NHL game, including event
players, coordinates, on-ice skaters, strength states, shot
distance/angle, and shift integration. Uses the new NHL API at
api-web.nhle.com.

## Usage

``` r
nhl_game_pbp(game_id, include_shifts = TRUE)
```

## Arguments

- game_id:

  Game unique ID (e.g. 2024020001)

- include_shifts:

  Logical; whether to integrate shift data for on-ice player tracking.
  Default TRUE.

## Value

A data frame with one row per event.

## Examples

``` r
# \donttest{
  try(nhl_game_pbp(game_id = 2024020001))
#> ── NHL Game PBP from NHL.com ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 06:28:10 UTC
#> # A tibble: 850 × 92
#>    event_type   event secondary_type event_team_abbr event_team_type description
#>    <chr>        <chr> <chr>          <chr>           <chr>           <glue>     
#>  1 CHANGE       Chan… NA             NA              NA              ON: Nicola…
#>  2 CHANGE       Chan… NA             NA              NA              ON: Jacob …
#>  3 FACEOFF      Face… NA             NJD             away            Nico Hisch…
#>  4 PERIOD_START Peri… NA             NA              NA              Start of P…
#>  5 HIT          Hit   NA             BUF             home            Beck Malen…
#>  6 SHOT         Shot  wrist          NJD             away            Simon Neme…
#>  7 GIVEAWAY     Give… NA             NJD             away            Giveaway b…
#>  8 CHANGE       Chan… NA             NA              NA              ON: Bowen …
#>  9 CHANGE       Chan… NA             NA              NA              ON: Timo M…
#> 10 CHANGE       Chan… NA             NA              NA              ON: Jesper…
#> # ℹ 840 more rows
#> # ℹ 86 more variables: period <int>, period_type <chr>, period_time <chr>,
#> #   period_seconds <dbl>, period_seconds_remaining <dbl>,
#> #   period_time_remaining <chr>, game_seconds <dbl>,
#> #   game_seconds_remaining <dbl>, home_score <int>, away_score <int>,
#> #   event_player_1_name <chr>, event_player_1_type <chr>,
#> #   event_player_1_id <int>, event_player_2_name <chr>, …
# }
```
