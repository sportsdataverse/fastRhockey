# **NHL Game Boxscore**

Retrieve boxscore data for a specific NHL game from the NHL web API
(`api-web.nhle.com`).

## Usage

``` r
nhl_game_boxscore(game_id)
```

## Arguments

- game_id:

  *(integer)* NHL game ID, e.g. `2024020001`.

## Value

A named `list` containing:

- **game_info** — one-row tibble with game metadata (id, season, game
  type, date, venue, teams, final score, shots on goal).

- **team_box** — two-row tibble (away / home) with team-level totals.

- **skater_stats** — tibble of all skater (forward + defense) individual
  stats for both teams.

- **goalie_stats** — tibble of all goalie individual stats for both
  teams.

## Details

Uses the endpoint
`https://api-web.nhle.com/v1/gamecenter/{game_id}/boxscore`.

## Examples

``` r
if (FALSE) { # \dontrun{
  box <- nhl_game_boxscore(2024020001)
  box$game_info
  box$skater_stats
} # }
```
