# **NHL All Players by Season**

Aggregator helper that iterates every NHL team's season roster (via
[`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md))
and flattens forwards, defensemen, and goalies into a single tidy data
frame. Mirrors the `Helpers.all_players` convenience helper from the
`nhl-api-py` Python client.

## Usage

``` r
nhl_all_players_by_season(season, sleep_rate = 0)
```

## Arguments

- season:

  Integer four-digit year representing the start year of the season
  (e.g. `2024` for the 2024-25 season), matching the convention used by
  [`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md).

- sleep_rate:

  Numeric seconds to
  [`Sys.sleep()`](https://rdrr.io/r/base/Sys.sleep.html) between
  per-team API calls. Defaults to `0` (no delay).

## Value

A `fastRhockey_data` / `data.frame` with one row per player across all
NHL rosters, including `player_id`, `first_name`, `last_name`,
`position_code`, `team_abbr`, and `season`. Returns `NULL` on outer
failure.

## Examples

``` r
# \donttest{
  try(nhl_all_players_by_season(season = 2024))
#> ── NHL All Players by Season ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:23 UTC
#> # A tibble: 853 × 15
#>    player_id first_name last_name full_name         sweater_number position_code
#>        <int> <chr>      <chr>     <chr>                      <int> <chr>        
#>  1   8479525 Ross       Colton    Ross Colton                   20 F            
#>  2   8477494 Jonathan   Drouin    Jonathan Drouin               27 F            
#>  3   8480835 Jack       Drury     Jack Drury                    18 F            
#>  4   8480448 Parker     Kelly     Parker Kelly                  17 F            
#>  5   8481641 Joel       Kiviranta Joel Kiviranta                94 F            
#>  6   8476455 Gabriel    Landeskog Gabriel Landeskog             92 F            
#>  7   8477476 Artturi    Lehkonen  Artturi Lehkonen              62 F            
#>  8   8477492 Nathan     MacKinnon Nathan MacKinnon              29 F            
#>  9   8480039 Martin     Necas     Martin Necas                  88 F            
#> 10   8475754 Brock      Nelson    Brock Nelson                  11 F            
#> # ℹ 843 more rows
#> # ℹ 9 more variables: shoots_catches <chr>, height_inches <int>,
#> #   weight_pounds <int>, birth_date <chr>, birth_city <chr>,
#> #   birth_country <chr>, headshot_url <chr>, team_abbr <chr>, season <dbl>
# }
```
