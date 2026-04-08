# **NHL Player Info**

Returns biographical and career information for an NHL player. Uses the
NHL API (`api-web.nhle.com`).

## Usage

``` r
nhl_player_info(player_id)
```

## Arguments

- player_id:

  Integer player ID (e.g., 8476899)

## Value

Returns a data frame with player biographical information.

## Examples

``` r
# \donttest{
  try(nhl_player_info(player_id = 8476899))
#> ── NHL Player Info ──────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 07:21:50 UTC
#> # A tibble: 1 × 22
#>   player_id first_name last_name full_name   team_abbr team_name  sweater_number
#>       <int> <chr>      <chr>     <chr>       <chr>     <chr>               <int>
#> 1   8476899 Matt       Murray    Matt Murray SEA       Seattle K…             30
#> # ℹ 15 more variables: position <chr>, shoots_catches <chr>,
#> #   height_inches <int>, weight_pounds <int>, birth_date <chr>,
#> #   birth_city <chr>, birth_state <chr>, birth_country <chr>, draft_year <int>,
#> #   draft_round <int>, draft_pick <int>, draft_overall <int>,
#> #   draft_team_abbr <chr>, is_active <lgl>, headshot_url <chr>
# }
```
