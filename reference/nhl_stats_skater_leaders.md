# **NHL Stats API — Skater Leaders**

Returns a skater leaderboard for a given statistic attribute from the
NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/leaders/skaters/{attribute}`).
Distinct from the api-web `skater-stats-leaders` endpoint.

## Usage

``` r
nhl_stats_skater_leaders(attribute, lang = "en", cayenne_exp = NULL)
```

## Arguments

- attribute:

  Character (required). The stat attribute to rank by. Known valid
  values include `"points"`, `"goals"`, `"assists"`. Note that this
  endpoint does **not** accept `start` / `limit` query parameters —
  passing them produces a 500. Use `cayenne_exp` to filter.

- lang:

  Character language code. Default `"en"`.

- cayenne_exp:

  Optional Cayenne filter expression string passed via the `cayenneExp`
  query parameter (e.g., `"seasonId=20242025"`).

## Value

A `fastRhockey_data` tibble of skater leaders, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_skater_leaders(attribute = "assists"))
#> ── NHL Stats Skater Leaders ─────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:03 UTC
#> # A tibble: 10 × 15
#>    assists player_id player_current_team_id player_first_name player_full_name
#>      <int>     <int> <lgl>                  <chr>             <chr>           
#>  1     163   8447400 NA                     Wayne             Wayne Gretzky   
#>  2     135   8447400 NA                     Wayne             Wayne Gretzky   
#>  3     125   8447400 NA                     Wayne             Wayne Gretzky   
#>  4     122   8447400 NA                     Wayne             Wayne Gretzky   
#>  5     121   8447400 NA                     Wayne             Wayne Gretzky   
#>  6     120   8447400 NA                     Wayne             Wayne Gretzky   
#>  7     118   8447400 NA                     Wayne             Wayne Gretzky   
#>  8     114   8448782 NA                     Mario             Mario Lemieux   
#>  9     114   8447400 NA                     Wayne             Wayne Gretzky   
#> 10     109   8447400 NA                     Wayne             Wayne Gretzky   
#> # ℹ 10 more variables: player_last_name <chr>, player_position_code <chr>,
#> #   player_sweater_number <int>, team_id <int>, team_franchise_id <int>,
#> #   team_full_name <chr>, team_league_id <int>, team_logos <list>,
#> #   team_raw_tricode <chr>, team_tri_code <chr>
# }
```
