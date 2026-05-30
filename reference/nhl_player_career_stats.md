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

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_id | integer | Unique player identifier. |
| first_name | character | Player first name. |
| last_name | character | Player last name. |
| position | character | Player position. |
| assists | integer | Assists. |
| game_type_id | integer | Game type the row belongs to. |
| games_played | integer | Games played. |
| goals | integer | Goals scored. |
| league_abbrev | character | League abbreviation. |
| pim | integer | Penalty minutes. |
| points | integer | Total points (goals + assists). |
| season | integer | Season (concluding year, YYYY). |
| sequence | integer | Sequence order of the season row. |
| game_winning_goals | integer | Game-winning goals. |
| plus_minus | integer | Plus/minus rating. |
| power_play_goals | integer | Power play goals. |
| shorthanded_goals | integer | Shorthanded goals. |
| shots | integer | Shots on goal. |
| avg_toi | character | Average time on ice. |
| faceoff_winning_pctg | numeric | Faceoff winning percentage. |
| ot_goals | integer | Overtime goals. |
| power_play_points | integer | Power play points. |
| shooting_pctg | numeric | Shooting percentage. |
| shorthanded_points | integer | Shorthanded points. |
| team_name_default | character | Team name (default locale). |
| team_name_cs | character | Team name (Czech locale). |
| team_name_de | character | Team name (German locale). |
| team_name_es | character | Team name (Spanish locale). |
| team_name_fi | character | Team name (Finnish locale). |
| team_name_sk | character | Team name (Slovak locale). |
| team_name_sv | character | Team name (Swedish locale). |
| team_name_fr | character | Team name (French locale). |
| team_common_name_default | character | Team common name (default locale). |
| team_common_name_cs | character | Team common name (Czech locale). |
| team_common_name_de | character | Team common name (German locale). |
| team_common_name_es | character | Team common name (Spanish locale). |
| team_common_name_fi | character | Team common name (Finnish locale). |
| team_common_name_sk | character | Team common name (Slovak locale). |
| team_common_name_sv | character | Team common name (Swedish locale). |
| team_place_name_with_preposition_default | character | Team place name with preposition. |
| team_place_name_with_preposition_fr | character | Team place name with preposition (FR). |

## Examples

``` r
# \donttest{
  try(nhl_player_career_stats(player_id = 8478402))
#> ── NHL Player Career Stats ──────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-30 03:09:39 UTC
#> # A tibble: 36 × 41
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
#> # ℹ 26 more rows
#> # ℹ 34 more variables: goals <int>, league_abbrev <chr>, pim <int>,
#> #   points <int>, season <int>, sequence <int>, game_winning_goals <int>,
#> #   plus_minus <int>, power_play_goals <int>, shorthanded_goals <int>,
#> #   shots <int>, avg_toi <chr>, faceoff_winning_pctg <dbl>, ot_goals <int>,
#> #   power_play_points <int>, shooting_pctg <dbl>, shorthanded_points <int>,
#> #   team_name_default <chr>, team_name_cs <chr>, team_name_de <chr>, …
# }
```
