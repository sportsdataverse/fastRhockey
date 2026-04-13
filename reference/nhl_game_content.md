# **NHL Game Content**

Returns game summary content for a given game ID. Uses the new NHL API
gamecenter landing endpoint (`api-web.nhle.com`).

**Note:** The original `statsapi.web.nhl.com` endpoint has been retired.
This function now returns game summary data (teams, scoring, three
stars) from the gamecenter landing page instead of the old
highlights/content data.

## Usage

``` r
nhl_game_content(game_id)
```

## Arguments

- game_id:

  Game unique ID (e.g., 2024020001)

## Value

Returns a data frame with game summary information including teams,
scores, venue, and period scoring details.

## Examples

``` r
# \donttest{
  try(nhl_game_content(game_id = 2024020001))
#> ── NHL Game Content Information from NHL.com ────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:14 UTC
#> # A tibble: 1 × 28
#>      game_id   season game_type game_date  venue away_team_abbrev away_team_name
#>        <int>    <int>     <int> <chr>      <chr> <chr>            <chr>         
#> 1 2024020001 20242025         2 2024-10-04 O2 C… NJD              Devils        
#> # ℹ 21 more variables: away_team_score <int>, away_team_sog <int>,
#> #   home_team_abbrev <chr>, home_team_name <chr>, home_team_score <int>,
#> #   home_team_sog <int>, game_state <chr>, period <int>, period_type <chr>,
#> #   star_1_player_id <int>, star_1_name <chr>, star_1_team_abbrev <chr>,
#> #   star_1_position <chr>, star_2_player_id <int>, star_2_name <chr>,
#> #   star_2_team_abbrev <chr>, star_2_position <chr>, star_3_player_id <int>,
#> #   star_3_name <chr>, star_3_team_abbrev <chr>, star_3_position <chr>
# }
```
