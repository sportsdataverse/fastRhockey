# **NHL Game Shifts**

Returns information on game shifts for a given game id

## Usage

``` r
nhl_game_shifts(game_id)
```

## Arguments

- game_id:

  Game unique ID

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(nhl_game_shifts(game_id = 2021020182))
#> ── NHL Game Shifts Information from NHL.com ─────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 07:33:35 UTC
#> # A tibble: 326 × 14
#>    event_team   period period_time period_seconds game_seconds num_on players_on
#>    <chr>         <int> <chr>                <dbl>        <dbl>  <int> <chr>     
#>  1 Boston Brui…      1 00:00                    0            0      6 Patrice B…
#>  2 Ottawa Sena…      1 00:00                    0            0      6 Matt Murr…
#>  3 Boston Brui…      1 00:18                   18           18      1 Matt Grze…
#>  4 Ottawa Sena…      1 00:18                   18           18      5 Michael D…
#>  5 Boston Brui…      1 00:41                   41           41      2 Craig Smi…
#>  6 Ottawa Sena…      1 00:46                   46           46      3 Thomas Ch…
#>  7 Boston Brui…      1 00:56                   56           56      2 Charlie C…
#>  8 Ottawa Sena…      1 00:58                   58           58      2 Zach Sanf…
#>  9 Boston Brui…      1 01:14                   74           74      5 Erik Haul…
#> 10 Ottawa Sena…      1 01:14                   74           74      5 Michael D…
#> # ℹ 316 more rows
#> # ℹ 7 more variables: ids_on <chr>, num_off <int>, players_off <chr>,
#> #   ids_off <chr>, event <chr>, event_type <chr>, game_seconds_remaining <dbl>
# }
```
