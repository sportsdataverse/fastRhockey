# **Ligue Magnus competitions (one per season)**

Competitions of a competition type from liguemagnus.com
(`get_competition_by_type`): one row per season, with the
`competition_id` the other routes key a season by. Coverage starts in
2020-21.

## Usage

``` r
magnus_competitions(competition_type = 1, return_parsed = TRUE)
```

## Arguments

- competition_type:

  Competition type: `1` = Ligue Magnus (default), `2` = Division 1.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON envelope as a list.

## Value

A tibble (`return_parsed = TRUE`), else the raw envelope list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| competition_id | integer | Ligue Magnus competition id (integer); one per season, the key get_phases and get_rencontres take (1 = 2020-21 ... 240 = 2026-27). |
| season | integer | Season by its END year (2027 = 2026-27), the federation's saison field. |
| competition | character | Competition name as published (sponsor name included, e.g., "Synerglace Ligue Magnus"). |
| competition_type_id | integer | Competition type id: 1 = Ligue Magnus, 2 = Division 1. |
| competition_type_code | character | Competition type code (NA for the Ligue Magnus). |
| competition_type | character | Competition type label (e.g., "Ligue Magnus"). |
| external_url | character | External site linked from the competition (streaming partner). |
| published | logical | Whether the competition is published on the league site. |

## Errors

`fastRhockey_no_data`: `success = false` with "Aucune donnee
disponible", an empty `data` stub, zero rows, a season outside 2020-21
onwards, an unknown or unplayed game. `fastRhockey_asset_fetch`: a
failed fetch, `success = false` with any other message, rows outside the
requested scope, a partial schedule page, or a game page without its
embedded game sheet. `fastRhockey_bad_request`: HTTP 400/422, a
validation error naming a rejected parameter, or an invalid argument.

## See also

Other Ligue Magnus Functions:
[`magnus_game()`](https://fastRhockey.sportsdataverse.org/reference/magnus_game.md),
[`magnus_lineups()`](https://fastRhockey.sportsdataverse.org/reference/magnus_lineups.md),
[`magnus_pbp()`](https://fastRhockey.sportsdataverse.org/reference/magnus_pbp.md),
[`magnus_phases()`](https://fastRhockey.sportsdataverse.org/reference/magnus_phases.md),
[`magnus_round_games()`](https://fastRhockey.sportsdataverse.org/reference/magnus_round_games.md),
[`magnus_rounds()`](https://fastRhockey.sportsdataverse.org/reference/magnus_rounds.md),
[`magnus_schedule()`](https://fastRhockey.sportsdataverse.org/reference/magnus_schedule.md),
[`magnus_shootout()`](https://fastRhockey.sportsdataverse.org/reference/magnus_shootout.md),
[`magnus_standings()`](https://fastRhockey.sportsdataverse.org/reference/magnus_standings.md),
[`magnus_teams()`](https://fastRhockey.sportsdataverse.org/reference/magnus_teams.md)

## Examples

``` r
# \donttest{
  try(magnus_competitions())
#> ── Ligue Magnus Competitions Information from liguemagnus.com ──────────────────
#> ℹ Data updated: 2026-10-13 01:56:44 UTC
#> # A tibble: 7 × 8
#>   competition_id season competition    competition_type_id competition_type_code
#>            <int>  <int> <chr>                        <int> <chr>                
#> 1              1   2021 Synerglace Li…                   1 NA                   
#> 2             34   2022 Synerglace Li…                   1 NA                   
#> 3             79   2023 Synerglace li…                   1 NA                   
#> 4            120   2024 Synerglace Li…                   1 NA                   
#> 5            157   2025 Synerglace Li…                   1 NA                   
#> 6            197   2026 Synerglace Li…                   1 NA                   
#> 7            240   2027 Synerglace Li…                   1 NA                   
#> # ℹ 3 more variables: competition_type <chr>, external_url <chr>,
#> #   published <lgl>
# }
```
