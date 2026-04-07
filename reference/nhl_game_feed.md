# **NHL Game Feed**

Returns a named list with play-by-play data plus game metadata for a
given NHL game. Uses the new NHL API at api-web.nhle.com.

## Usage

``` r
nhl_game_feed(game_id, include_shifts = TRUE)
```

## Arguments

- game_id:

  Game unique ID (e.g. 2024020001)

- include_shifts:

  Logical; whether to integrate shift data for on-ice player tracking.
  Default TRUE.

## Value

A named list with elements: pbp (play-by-play data frame), game_info
(game metadata), rosters (player roster data frame)

## Examples

``` r
# \donttest{
  try(nhl_game_feed(game_id = 2024020001))
#> $pbp
#> ── NHL Game PBP from NHL.com ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 06:45:29 UTC
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
#> 
#> $game_info
#> # A tibble: 1 × 10
#>      game_id   season game_type game_date  venue   home_team_abbr away_team_abbr
#>        <int>    <int> <chr>     <chr>      <chr>   <chr>          <chr>         
#> 1 2024020001 20242025 R         2024-10-04 O2 Cze… BUF            NJD           
#> # ℹ 3 more variables: home_score <int>, away_score <int>, game_state <chr>
#> 
#> $rosters
#> # A tibble: 40 × 8
#>    player_id full_name      first_name last_name team_abbr team_id position_code
#>        <int> <chr>          <chr>      <chr>     <chr>       <int> <chr>        
#>  1   8474593 Jacob Markstr… Jacob      Markstrom NJD             1 G            
#>  2   8474596 Jake Allen     Jake       Allen     NJD             1 G            
#>  3   8475193 Tomas Tatar    Tomas      Tatar     NJD             1 L            
#>  4   8475287 Erik Haula     Erik       Haula     NJD             1 L            
#>  5   8475455 Brenden Dillon Brenden    Dillon    NJD             1 D            
#>  6   8475722 Jason Zucker   Jason      Zucker    BUF             7 L            
#>  7   8476292 Ondrej Palat   Ondrej     Palat     NJD             1 L            
#>  8   8476462 Dougie Hamilt… Dougie     Hamilton  NJD             1 D            
#>  9   8476474 Stefan Noesen  Stefan     Noesen    NJD             1 R            
#> 10   8477365 Connor Clifton Connor     Clifton   BUF             7 D            
#> # ℹ 30 more rows
#> # ℹ 1 more variable: sweater_number <int>
#> 
# }
```
