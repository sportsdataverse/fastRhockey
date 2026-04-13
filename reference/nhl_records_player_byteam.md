# **NHL Records - Players by Team**

Returns every player who has suited up for a given team via the NHL
Records API
(`https://records.nhl.com/site/api/player/byTeam/{team_id}`).

## Usage

``` r
nhl_records_player_byteam(team_id, cayenne_exp = NULL)
```

## Arguments

- team_id:

  Integer team ID (required).

- cayenne_exp:

  Optional Cayenne filter expression string.

## Value

A `fastRhockey_data` tibble of players, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_player_byteam(team_id = 10))
#> ── NHL Records Player by Team ───────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:45 UTC
#> # A tibble: 72 × 71
#>         id accrued_seasons add_names age_sign_waiver age_signel_fa alert
#>      <int>           <int> <lgl>               <int>         <int> <chr>
#>  1 8475690              13 NA                     21            20 N    
#>  2 8476329               6 NA                     22            22 N    
#>  3 8476988               8 NA                     22            22 Y    
#>  4 8477541               2 NA                     21            21 N    
#>  5 8479528              NA NA                     NA            NA N    
#>  6 8479599              NA NA                     NA            NA N    
#>  7 8479968               2 NA                     22            22 N    
#>  8 8480284              NA NA                     NA            NA N    
#>  9 8480870               1 NA                     19            19 N    
#> 10 8480977               0 NA                     18            18 N    
#> # ℹ 62 more rows
#> # ℹ 65 more variables: birth_city <chr>, birth_country <chr>, birth_date <chr>,
#> #   birth_state_province <chr>, career_team_id <lgl>,
#> #   central_registry_position <chr>, club_elec_arb <chr>,
#> #   current_team_id <int>, date_of_death <lgl>, dda_id <int>, deceased <lgl>,
#> #   ep_player_id <int>, fa_group_after_season <lgl>, first_name <chr>,
#> #   first_signed_by_team_id <int>, free_agent_group <chr>, full_name <chr>, …
# }
```
