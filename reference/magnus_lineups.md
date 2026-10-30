# **Ligue Magnus game roster**

Game roster from the game sheet: one row per dressed player, with the
person id, the per-game participant id, jersey, licence number,
position, captaincy and MVP flag. `player_id` (the person) is stable
across games; `player_participant_id` is this game's roster entry, the
key the sheet's on-ice lists and MVPs use.

## Usage

``` r
magnus_lineups(game_id, return_parsed = TRUE)
```

## Arguments

- game_id:

  Game id (`game_id` of
  [`magnus_schedule()`](https://fastRhockey.sportsdataverse.org/reference/magnus_schedule.md)),
  e.g. 77779 (2026-27, shootout and MVPs); also sampled: 68838, 865.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  game sheet extracted from the page's embedded JSON, as a list.

## Value

A tibble (`return_parsed = TRUE`; zero rows for an empty sheet), else
the game sheet list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | integer | Game id (integer join key). |
| team_id | integer | Team id (integer). |
| home_away | character | The team's side: home or away. |
| player_id | integer | Person id (integer; joueurs\[\].id = personne.id), stable across games; the player key of every Magnus frame. |
| player_participant_id | integer | The player's roster entry in THIS game (integer; changes every game), the key the sheet's on-ice lists and MVPs use; never join across games on it. |
| player_name | character | Player name (SURNAME First). |
| jersey_number | character | Jersey number worn in this game (string as published). |
| registered_jersey_number | character | Registered jersey number of the person; can differ from the one worn. |
| licence_number | character | FFHG licence number (string; keeps leading zeros). |
| position_code | character | Position code: A (attaquant), D (défenseur), GB (gardien de but). |
| position | character | Position in English: forward, defence or goalie. |
| captaincy | character | C (captain) or A (alternate captain); null otherwise. |
| is_mvp | logical | Whether the player was named his team's MVP of the game. |
| photo_url | character | Photo thumbnail URL of the player. |

## Errors

As
[`magnus_competitions()`](https://fastRhockey.sportsdataverse.org/reference/magnus_competitions.md).

## See also

Other Ligue Magnus Functions:
[`magnus_competitions()`](https://fastRhockey.sportsdataverse.org/reference/magnus_competitions.md),
[`magnus_game()`](https://fastRhockey.sportsdataverse.org/reference/magnus_game.md),
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
  try(magnus_lineups(game_id = 77779))
#> ── Ligue Magnus Lineups Information from liguemagnus.com ───────────────────────
#> ℹ Data updated: 2026-10-30 00:17:08 UTC
#> # A tibble: 41 × 14
#>    game_id team_id home_away player_id player_participant_id player_name        
#>      <int>   <int> <chr>         <int>                 <int> <chr>              
#>  1   77779     553 home          66741                618664 VILLARD Fiorenzo   
#>  2   77779     553 home         120434                618665 PRAPAVESSIS Matthew
#>  3   77779     553 home         137034                618666 CENTAZZO Orrin     
#>  4   77779     553 home          99599                618667 HALLEY Philippe    
#>  5   77779     553 home          34461                618668 TAVERNIER Sami     
#>  6   77779     553 home          43372                618669 MELIN Vincent      
#>  7   77779     553 home          57256                618670 ONNO Lucien        
#>  8   77779     553 home         107434                618671 MANNING Neil       
#>  9   77779     553 home          59613                618672 HERVE Jordan       
#> 10   77779     553 home          25980                618673 GUTIERREZ Romain   
#> # ℹ 31 more rows
#> # ℹ 8 more variables: jersey_number <chr>, registered_jersey_number <chr>,
#> #   licence_number <chr>, position_code <chr>, position <chr>, captaincy <chr>,
#> #   is_mvp <lgl>, photo_url <chr>
# }
```
