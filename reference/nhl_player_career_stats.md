# **NHL Player Career Stats**

Aggregator helper that combines biographical information from the NHL
player landing endpoint with the player's season-by-season career
totals. Returns a single multi-row "career-by-season" data frame.
Mirrors the `Stats.player_career_stats` convenience helper from the
`nhl-api-py` Python client.

## Usage

``` r
nhl_player_career_stats(player_id)
```

## Arguments

- player_id:

  Integer player ID (e.g. `8478402` for Connor McDavid).

## Value

A `fastRhockey_data` / `data.frame` with one row per season played.
Always includes `player_id`, `first_name`, `last_name`, `position`,
`season`, `game_type_id`, plus the season stat columns exposed by the
landing endpoint's `seasonTotals` payload (e.g. `goals`, `assists`,
`points`, `games_played`, `pim`, `shots`, `team_name_default`,
`league_abbrev`, ...). Returns `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_player_career_stats(player_id = 8478402))
#> ── NHL Player Career Stats ──────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:22 UTC
#> # A tibble: 35 × 41
#>    player_id first_name last_name position assists game_type_id games_played
#>        <int> <chr>      <chr>     <chr>      <int>        <int>        <int>
#>  1   8478402 Connor     McDavid   C              7            2            7
#>  2   8478402 Connor     McDavid   C             50            2           33
#>  3   8478402 Connor     McDavid   C             65            2           41
#>  4   8478402 Connor     McDavid   C             32            2           17
#>  5   8478402 Connor     McDavid   C             15            3           14
#>  6   8478402 Connor     McDavid   C             41            2           63
#>  7   8478402 Connor     McDavid   C              6            2            7
#>  8   8478402 Connor     McDavid   C             71            2           56
#>  9   8478402 Connor     McDavid   C             15            3           14
#> 10   8478402 Connor     McDavid   C             76            2           47
#> # ℹ 25 more rows
#> # ℹ 34 more variables: goals <int>, league_abbrev <chr>, pim <int>,
#> #   points <int>, season <int>, sequence <int>, game_winning_goals <int>,
#> #   plus_minus <int>, power_play_goals <int>, shorthanded_goals <int>,
#> #   shots <int>, avg_toi <chr>, faceoff_winning_pctg <dbl>, ot_goals <int>,
#> #   power_play_points <int>, shooting_pctg <dbl>, shorthanded_points <int>,
#> #   team_name_default <chr>, team_name_cs <chr>, team_name_de <chr>, …
# }
```
