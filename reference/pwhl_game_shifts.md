# **PWHL Game Shifts**

All player shifts for a PWHL game (one row per stint), from HockeyTech.
Calls the HockeyTech `modulekit/gameshifts` endpoint and returns one row
per player-shift stint via the internal `.parse_hockeytech_shifts()`
helper.

## Usage

``` r
pwhl_game_shifts(game_id)
```

## Arguments

- game_id:

  PWHL game id (integer or character).

## Value

A `fastRhockey_data` data frame with one row per shift stint:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_id | numeric | Game identifier (echoed from argument). |
| player_id | numeric | Player unique identifier. |
| first_name | character | Player first name. |
| last_name | character | Player last name. |
| jersey_number | character | Player jersey number. |
| home | integer | 1 if home player, 0 if visitor. |
| period | integer | Period of the shift. |
| start_time | character | Shift start time (MM:SS countdown clock). |
| end_time | character | Shift end time (MM:SS countdown clock). |
| length | character | Shift length string. |
| start_s | integer | Shift start in countdown seconds. |
| end_s | integer | Shift end in countdown seconds. |
| goal_on_shift | integer | 1 if a goal occurred during this shift, else 0. |
| penalty_on_shift | integer | 1 if a penalty occurred during this shift, else 0. |

## See also

Other PWHL Functions:
[`pwhl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_corsi.md),
[`pwhl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_toi.md)

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(pwhl_game_shifts(game_id = 42))
#> ── PWHL Game Shifts data from HockeyTech ────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 22:26:34 UTC
#> # A tibble: 730 × 14
#>    game_id player_id first_name last_name jersey_number  home period start_time
#>      <dbl>     <int> <chr>      <chr>     <chr>         <int>  <int> <chr>     
#>  1      42        27 Jillian    Dempsey   14                1      1 03:16     
#>  2      42        27 Jillian    Dempsey   14                1      1 08:49     
#>  3      42        27 Jillian    Dempsey   14                1      1 12:46     
#>  4      42        27 Jillian    Dempsey   14                1      1 17:28     
#>  5      42        27 Jillian    Dempsey   14                1      2 04:21     
#>  6      42        27 Jillian    Dempsey   14                1      2 06:59     
#>  7      42        27 Jillian    Dempsey   14                1      2 10:41     
#>  8      42        27 Jillian    Dempsey   14                1      2 16:32     
#>  9      42        27 Jillian    Dempsey   14                1      3 00:16     
#> 10      42        27 Jillian    Dempsey   14                1      3 04:23     
#> # ℹ 720 more rows
#> # ℹ 6 more variables: end_time <chr>, length <chr>, start_s <int>, end_s <int>,
#> #   goal_on_shift <int>, penalty_on_shift <int>
# }
```
