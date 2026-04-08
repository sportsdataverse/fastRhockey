# **NHL Stats API — Seasons List**

Returns a list of all seasons from the Stats REST API.

## Usage

``` r
nhl_stats_seasons(lang = "en")
```

## Arguments

- lang:

  Character language code. Default "en".

## Value

Returns a data frame with season data.

## Examples

``` r
# \donttest{
  try(nhl_stats_seasons())
#> ── NHL Stats Seasons ────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 07:33:46 UTC
#> # A tibble: 108 × 23
#>          id all_star_game_in_use conferences_in_use divisions_in_use end_date   
#>       <int>                <int>              <int>            <int> <chr>      
#>  1 19531954                    1                  0                0 1954-04-16…
#>  2 20162017                    1                  1                1 2017-06-11…
#>  3 19421943                    0                  0                0 1943-04-08…
#>  4 19231924                    0                  0                0 1924-03-25…
#>  5 19441945                    0                  0                0 1945-04-22…
#>  6 19451946                    0                  0                0 1946-04-09…
#>  7 19981999                    1                  1                1 1999-06-19…
#>  8 19691970                    1                  0                1 1970-05-10…
#>  9 19321933                    0                  0                1 1933-04-13…
#> 10 19461947                    1                  0                0 1947-04-19…
#> # ℹ 98 more rows
#> # ℹ 18 more variables: entry_draft_in_use <int>, formatted_season_id <chr>,
#> #   minimum_playoff_minutes_for_goalie_stats_leaders <int>,
#> #   minimum_regular_games_for_goalie_stats_leaders <int>,
#> #   nhl_stanley_cup_owner <int>, number_of_games <int>,
#> #   olympics_participation <int>, point_for_ot_loss_in_use <int>,
#> #   preseason_startdate <chr>, regular_season_end_date <chr>, …
# }
```
