# **PWHL Schedule**

PWHL Schedule lookup

## Usage

``` r
pwhl_schedule(season, game_type = "regular")
```

## Arguments

- season:

  Season (YYYY) to pull the schedule from, the concluding year in
  XXXX-YY format

- game_type:

  Game type: "regular" (default), "preseason", or "playoffs".

## Value

A data frame with schedule data including columns: game_id, season,
game_date, game_status, home_team, home_team_id, away_team,
away_team_id, home_score, away_score, winner, venue, venue_url.

## Examples

``` r
# \donttest{
  try(pwhl_schedule(season = 2023))
#> Error in parse_url(url) : length(url) == 1 is not TRUE
# }
```
