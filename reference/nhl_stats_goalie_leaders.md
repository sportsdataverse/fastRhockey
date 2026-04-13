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

A `fastRhockey_data` tibble of goalie leaders, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_goalie_leaders(attribute = "savePctg"))
#> ── NHL Stats Goalie Leaders ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:00 UTC
#> # A tibble: 10 × 15
#>    save_pctg player_id player_current_team_id player_first_name player_full_name
#>        <dbl>     <int>                  <int> <chr>             <chr>           
#>  1      1.41   8449959                     NA Bruce             Bruce Gamble    
#>  2      1      8484910                     55 Victor            Victor Ostman   
#>  3      1      8484293                     NA Yaniv             Yaniv Perets    
#>  4      1      8483575                     18 Matt              Matt Murray     
#>  5      1      8483158                     NA Matthew           Matthew Berlin  
#>  6      1      8481033                     54 Akira             Akira Schmid    
#>  7      1      8480313                     15 Logan             Logan Thompson  
#>  8      1      8480022                      6 Michael           Michael DiPietro
#>  9      1      8479979                     25 Jake              Jake Oettinger  
#> 10      1      8479288                     NA Kasimir           Kasimir Kaskisuo
#> # ℹ 10 more variables: player_last_name <chr>, player_position_code <chr>,
#> #   player_sweater_number <int>, team_id <int>, team_franchise_id <int>,
#> #   team_full_name <chr>, team_league_id <int>, team_logos <list>,
#> #   team_raw_tricode <chr>, team_tri_code <chr>
# }
```
