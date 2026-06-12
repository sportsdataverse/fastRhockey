# **PWHL Game Corsi/Fenwick (player-level on-ice)**

Player-level on-ice shot-attempt metrics for a PWHL game. Corsi = shot +
blocked_shot + goal; Fenwick excludes blocked shots. Missed shots are
not present in the HockeyTech feed, so both are proxies
(`corsi_includes_missed = FALSE`). TOI is left-joined from
[`pwhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_shifts.md)
so every on-ice player is retained even when shift data is absent
(resulting in `NA` `toi_seconds`).

## Usage

``` r
pwhl_game_corsi(game_id)
```

## Arguments

- game_id:

  PWHL game id (integer or character).

## Value

A `fastRhockey_data` data frame with one row per on-ice player:

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_id | character | Player unique identifier. |
| corsi_for | integer | On-ice shot attempts for (shot + blocked_shot + goal). |
| corsi_against | integer | On-ice shot attempts against. |
| corsi_for_pct | numeric | Corsi for percentage (NA when total = 0). |
| fenwick_for | integer | On-ice unblocked shot attempts for (shot + goal). |
| fenwick_against | integer | On-ice unblocked shot attempts against. |
| fenwick_for_pct | numeric | Fenwick for percentage (NA when total = 0). |
| corsi_includes_missed | logical | Always FALSE (no missed-shot events in the HockeyTech feed). |
| toi_seconds | integer | Total time on ice in seconds (NA if not in shift data). |
| corsi_for_per60 | numeric | Corsi For per 60 minutes (NA when toi_seconds is 0 or NA). |

## See also

Other PWHL Functions:
[`pwhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_shifts.md),
[`pwhl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_toi.md)

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(pwhl_game_corsi(game_id = 42))
#> ── PWHL Game Corsi from HockeyTech ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 13:23:08 UTC
#> # A tibble: 39 × 10
#>    player_id corsi_for corsi_against corsi_for_pct fenwick_for fenwick_against
#>    <chr>         <int>         <int>         <dbl>       <int>           <int>
#>  1 30               13            17         0.433          10              12
#>  2 84               21            18         0.538          17              15
#>  3 85               49            46         0.516          38              32
#>  4 157              22            12         0.647          18              10
#>  5 161              17            11         0.607          13               8
#>  6 163              14            19         0.424          12              15
#>  7 2                15            23         0.395          12              19
#>  8 6                31            32         0.492          23              25
#>  9 8                14            10         0.583          10               8
#> 10 16               12             9         0.571           7               6
#> # ℹ 29 more rows
#> # ℹ 4 more variables: fenwick_for_pct <dbl>, corsi_includes_missed <lgl>,
#> #   toi_seconds <int>, corsi_for_per60 <dbl>
# }
```
