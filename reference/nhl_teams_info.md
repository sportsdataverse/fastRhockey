# **NHL Teams Info**

Returns NHL team information for a given team abbreviation. Uses the new
NHL API via
[`nhl_teams`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams.md).

**Breaking change:** The old `team_id` (integer) parameter has been
replaced by `team_abbr` (3-letter string, e.g., "TBL") because the new
NHL API no longer exposes numeric team IDs as a primary identifier.

## Usage

``` r
nhl_teams_info(team_abbr)
```

## Arguments

- team_abbr:

  Three-letter team abbreviation (e.g., "TBL", "TOR", "SEA")

## Value

Returns a data frame with team information filtered to the given team.

## Examples

``` r
# \donttest{
  try(nhl_teams_info(team_abbr = "TBL"))
#> ── NHL Teams Information from NHL.com ───────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 06:13:54 UTC
#> # A tibble: 1 × 20
#>   team_abbr team_name team_common_name team_logo conference_abbr conference_name
#>   <chr>     <chr>     <chr>            <chr>     <chr>           <chr>          
#> 1 TBL       Tampa Ba… Lightning        https://… Eastern         Eastern        
#> # ℹ 14 more variables: division_abbr <chr>, division_name <chr>,
#> #   place_name <chr>, games_played <int>, wins <int>, losses <int>,
#> #   ot_losses <int>, points <int>, point_pctg <dbl>, goals_for <int>,
#> #   goals_against <int>, goal_differential <int>, streak_code <chr>,
#> #   streak_count <int>
# }
```
