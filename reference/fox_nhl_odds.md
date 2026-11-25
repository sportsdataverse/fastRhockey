# **Get Fox Sports NHL game odds**

**Get Fox Sports NHL game odds**

## Usage

``` r
fox_nhl_odds(game_id)
```

## Arguments

- game_id:

  Fox Bifrost event id.

## Value

A `fastRhockey_data` tibble, one row per team (`game_id`, `team`, plus
six-pack odds columns). Empty when no market is posted.

## Examples

``` r
 try(fox_nhl_odds("44398")) 
#> ✖ 2026-11-25 20:19:08.119996: Invalid arguments or no Fox nhl odds data available!
#> ✖ Args: sport = "nhl", resource = "odds", game_id = "44398", team_id = NULL, category = "scoring", who = "player", page = 0
#> ✖ Error: HTTP 404 Not Found from https://api.foxsports.com/bifrost/v1/nhl/event/44398/odds
#> data frame with 0 columns and 0 rows
```
