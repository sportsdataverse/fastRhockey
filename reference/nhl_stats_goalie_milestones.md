# **NHL Stats API — Goalie Milestones**

Returns goalie milestone achievements from the NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/milestones/goalies`).

## Usage

``` r
nhl_stats_goalie_milestones(
  lang = "en",
  cayenne_exp = NULL,
  limit = 100,
  start = 0
)
```

## Arguments

- lang:

  Character language code. Default `"en"`.

- cayenne_exp:

  Optional Cayenne filter expression string passed via the `cayenneExp`
  query parameter.

- limit:

  Integer maximum number of results. Default 100.

- start:

  Integer pagination start index. Default 0.

## Value

A `fastRhockey_data` tibble of goalie milestones, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_goalie_milestones())
#> ── NHL Stats Goalie Milestones ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:00 UTC
#> # A tibble: 52 × 17
#>       id current_team_id first_name game_type_id games_played last_name 
#>    <int>           <int> <chr>             <int>        <int> <chr>     
#>  1   390               3 Jonathan              3           92 Quick     
#>  2   391               3 Jonathan              3           92 Quick     
#>  3   392               3 Jonathan              3           92 Quick     
#>  4   422              24 Petr                  2          438 Mrazek    
#>  5   467              12 Frederik              2          552 Andersen  
#>  6   535              18 Juuse                 2          466 Saros     
#>  7   549               3 Igor                  3           44 Shesterkin
#>  8   595              55 Philipp               3           47 Grubauer  
#>  9   607              26 Darcy                 2          488 Kuemper   
#> 10   623              12 Frederik              3           85 Andersen  
#> # ℹ 42 more rows
#> # ℹ 11 more variables: milestone <chr>, milestone_amount <int>,
#> #   player_full_name <chr>, player_id <int>, so <int>, team_abbrev <chr>,
#> #   team_common_name <chr>, team_full_name <chr>, team_place_name <chr>,
#> #   toi_minutes <int>, wins <int>
# }
```
