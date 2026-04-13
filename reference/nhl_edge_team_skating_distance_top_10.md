# **NHL Edge Team Skating Distance Top 10**

Returns the NHL Edge top-10 team skating-distance leaderboard. Wraps
`https://api-web.nhle.com/v1/edge/team-skating-distance-top-10/{positions}/{strength}/{sortBy}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_team_skating_distance_top_10(
  positions,
  strength,
  sort_by,
  season = NULL,
  game_type = 2
)
```

## Arguments

- positions:

  Character position filter (e.g., `"F"`, `"D"`, `"all"`).

- strength:

  Character strength state (e.g., `"all"`, `"ev"`, `"pp"`, `"pk"`).

- sort_by:

  Character metric to sort the leaderboard by (e.g., `"total"`,
  `"per_60"`).

- season:

  Optional 4-digit end-year (e.g., `2025` for the 2024-25 season), an
  8-character API season (e.g., `"20242025"`), or `NULL` (default) for
  the current season via the `/now` endpoint.

- game_type:

  Integer game type. 1 = preseason, 2 = regular season (default), 3 =
  playoffs.

## Value

A `fastRhockey_data` tibble with the top-10 team leaderboard, or `NULL`
on failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_team_skating_distance_top_10(
    positions = "F",
    strength = "all",
    sort_by = "total"
  ))
#> ── NHL Edge Team Skating Distance Top 10 ────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:09 UTC
#> # A tibble: 10 × 35
#>    team_abbrev team_slug           team_common_name_def…¹ team_place_name_with…²
#>    <chr>       <chr>               <chr>                  <chr>                 
#>  1 NJD         new-jersey-devils-1 Devils                 New Jersey            
#>  2 PIT         pittsburgh-penguin… Penguins               Pittsburgh            
#>  3 COL         colorado-avalanche… Avalanche              Colorado              
#>  4 SEA         seattle-kraken-55   Kraken                 Seattle               
#>  5 FLA         florida-panthers-13 Panthers               Florida               
#>  6 PHI         philadelphia-flyer… Flyers                 Philadelphia          
#>  7 VGK         vegas-golden-knigh… Golden Knights         Vegas                 
#>  8 CGY         calgary-flames-20   Flames                 Calgary               
#>  9 DET         detroit-red-wings-… Red Wings              Detroit               
#> 10 CAR         carolina-hurricane… Hurricanes             Carolina              
#> # ℹ abbreviated names: ¹​team_common_name_default,
#> #   ²​team_place_name_with_preposition_default
#> # ℹ 31 more variables: team_team_logo_light <chr>, team_team_logo_dark <chr>,
#> #   distance_total_imperial <dbl>, distance_total_metric <dbl>,
#> #   distance_per60_imperial <dbl>, distance_per60_metric <dbl>,
#> #   distance_max_per_game_imperial <dbl>, distance_max_per_game_metric <dbl>,
#> #   distance_max_per_game_overlay_game_date <chr>, …
# }
```
