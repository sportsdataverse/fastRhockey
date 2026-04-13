# **NHL Records - Player Listing**

Returns the player listing from the NHL Records API
(`https://records.nhl.com/site/api/player`). Optionally filter to a
single player via `player_id` (switches resource to
`player/{player_id}`) or pass an arbitrary Cayenne expression.

## Usage

``` r
nhl_records_player(player_id = NULL, cayenne_exp = NULL)
```

## Arguments

- player_id:

  Optional integer player ID. If supplied, the resource becomes
  `player/{player_id}`.

- cayenne_exp:

  Optional Cayenne filter expression string. Ignored when `player_id` is
  supplied (single-player endpoint).

## Value

A `fastRhockey_data` tibble of players, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_player())
#> ── NHL Records Player ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:44 UTC
#> # A tibble: 23,300 × 71
#>         id accrued_seasons add_names         age_sign_waiver age_signel_fa alert
#>      <int>           <int> <chr>                       <int>         <int> <chr>
#>  1 8444850              NA "Henry"                        NA            NA N    
#>  2 8444851              NA "Gordon"                       NA            NA N    
#>  3 8444852              NA "Ron"                          NA            NA N    
#>  4 8444853              NA "Norm"                         NA            NA N    
#>  5 8444854              NA "Reg"                          NA            NA N    
#>  6 8444855              NA "Clarence \"Taff…              NA            NA N    
#>  7 8444856              NA "Gerry"                        NA            NA N    
#>  8 8444857              NA "Sid"                          NA            NA N    
#>  9 8444858              NA "Gene"                         NA            NA N    
#> 10 8444859              NA "Doug"                         NA            NA N    
#> # ℹ 23,290 more rows
#> # ℹ 65 more variables: birth_city <chr>, birth_country <chr>, birth_date <chr>,
#> #   birth_state_province <chr>, career_team_id <int>,
#> #   central_registry_position <chr>, club_elec_arb <chr>,
#> #   current_team_id <int>, date_of_death <chr>, dda_id <int>, deceased <lgl>,
#> #   ep_player_id <int>, fa_group_after_season <lgl>, first_name <chr>,
#> #   first_signed_by_team_id <int>, free_agent_group <chr>, full_name <chr>, …
# }
```
