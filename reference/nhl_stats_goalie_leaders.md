# **NHL Stats API — Goalie Leaders**

Returns a goalie leaderboard for a given statistic attribute from the
NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/leaders/goalies/{attribute}`).

## Usage

``` r
nhl_stats_goalie_leaders(attribute, lang = "en", cayenne_exp = NULL)
```

## Arguments

- attribute:

  Character (required). The stat attribute to rank by. Known valid
  values: `"savePctg"`, `"gaa"`, `"shutouts"`. Note that `"wins"`,
  `"saves"`, `"shotsAgainst"`, and `"goalsAgainst"` return 500 errors.
  The endpoint also does **not** accept `start` / `limit` query
  parameters — passing them produces a 500.

- lang:

  Character language code. Default `"en"`.

- cayenne_exp:

  Optional Cayenne filter expression string passed via the `cayenneExp`
  query parameter (e.g., `"seasonId=20242025"`).

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                        |           |                                   |
|------------------------|-----------|-----------------------------------|
| col_name               | types     | description                       |
| save_pctg              | numeric   | Save percentage.                  |
| player_id              | integer   | Unique player identifier.         |
| player_current_team_id | integer   | Player's current team identifier. |
| player_first_name      | character | Player first name.                |
| player_full_name       | character | Player full name.                 |
| player_last_name       | character | Player last name.                 |
| player_position_code   | character | Player position code.             |
| player_sweater_number  | integer   | Player jersey number.             |
| team_id                | integer   | Unique team identifier.           |
| team_franchise_id      | integer   | Team franchise identifier.        |
| team_full_name         | character | Full team name.                   |
| team_league_id         | integer   | League identifier.                |
| team_logos             | list      | Team logo assets.                 |
| team_raw_tricode       | character | Raw team tri-code.                |
| team_tri_code          | character | Team tri-code abbreviation.       |

## Examples

``` r
# \donttest{
  try(nhl_stats_goalie_leaders(attribute = "savePctg"))
#> ── NHL Stats Goalie Leaders ────────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:18:51 UTC
#> # A tibble: 10 × 15
#>    save_pctg player_id player_current_team_id player_first_name player_full_name
#>        <dbl>     <int> <lgl>                  <chr>             <chr>           
#>  1         1   8445249 NA                     Murray            Murray Bannerman
#>  2         1   8445457 NA                     Marc              Marc Behrend    
#>  3         1   8446087 NA                     Chris             Chris Clifford  
#>  4         1   8446306 NA                     Marc              Marc D'amour    
#>  5         1   8446368 NA                     Matt              Matt Delguidice 
#>  6         1   8446488 NA                     Dave              Dave Dryden     
#>  7         1   8446602 NA                     Gary              Gary Edwards    
#>  8         1   8446723 NA                     Randy             Randy Exelby    
#>  9         1   8446991 NA                     Grant             Grant Fuhr      
#> 10         1   8447066 NA                     John              John Garrett    
#> # ℹ 10 more variables: player_last_name <chr>, player_position_code <chr>,
#> #   player_sweater_number <int>, team_id <int>, team_franchise_id <int>,
#> #   team_full_name <chr>, team_league_id <int>, team_logos <list>,
#> #   team_raw_tricode <chr>, team_tri_code <chr>
# }
```
