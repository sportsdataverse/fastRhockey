# **PWHL Player Time On Ice**

Per-player time-on-ice totals for a PWHL game. Fetches all shift stints
via
[`pwhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_shifts.md)
then aggregates them with the internal `hockeytech_player_toi()` helper.

## Usage

``` r
pwhl_player_toi(game_id)
```

## Arguments

- game_id:

  PWHL game id (integer or character).

## Value

A `fastRhockey_data` data frame with one row per player, sorted
descending by `toi_seconds`:

|             |           |                                    |
|-------------|-----------|------------------------------------|
| col_name    | types     | description                        |
| player_id   | numeric   | Player unique identifier.          |
| first_name  | character | Player first name.                 |
| last_name   | character | Player last name.                  |
| toi_seconds | integer   | Total time on ice in seconds.      |
| num_shifts  | integer   | Number of individual shift stints. |
| avg_shift_s | numeric   | Average shift length in seconds.   |

## See also

Other PWHL Functions:
[`pwhl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_corsi.md),
[`pwhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_shifts.md)

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(pwhl_player_toi(game_id = 42))
#> ── PWHL Player TOI from HockeyTech ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 14:21:12 UTC
#> # A tibble: 39 × 6
#>    player_id first_name last_name toi_seconds num_shifts avg_shift_s
#>        <int> <chr>      <chr>           <int>      <int>       <dbl>
#>  1        85 Elaine     Chuli            3584          4       896  
#>  2         6 Aerin      Frankel          2400          2      1200  
#>  3        83 Erin       Ambrose          1534         29        52.9
#>  4        12 Megan      Keller           1478         26        56.8
#>  5        84 Kati       Tabin            1371         26        52.7
#>  6        15 Alina      Müller           1342         24        55.9
#>  7        78 Susanna    Tapani           1326         23        57.7
#>  8       163 Catherine  Daoust           1292         22        58.7
#>  9        32 Laura      Stacey           1266         27        46.9
#> 10        99 Mariah     Keopple          1254         26        48.2
#> # ℹ 29 more rows
# }
```
