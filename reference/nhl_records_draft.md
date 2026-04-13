# **NHL Records - Draft Listing**

Returns the NHL Entry Draft listing from the NHL Records API
(`https://records.nhl.com/site/api/draft`). Supports Cayenne filtering
and pagination.

## Usage

``` r
nhl_records_draft(cayenne_exp = NULL, limit = NULL, start = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string (e.g. `"draftYear=2015"`).

- limit:

  Optional integer page size.

- start:

  Optional integer pagination offset.

## Value

A `fastRhockey_data` tibble of draft picks, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_draft(cayenne_exp = "draftYear=2020", limit = 5))
#> ── NHL Records Draft ────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:27 UTC
#> # A tibble: 5 × 31
#>      id age_in_days age_in_days_for_year age_in_years amateur_club_name
#>   <int>       <int>                <int>        <int> <chr>            
#> 1 18392        6935                  361           18 Rimouski         
#> 2 18393        6623                   48           18 Sudbury          
#> 3 18394        6839                  265           18 Mannheim         
#> 4 18395        6767                  192           18 Frolunda         
#> 5 18396        6665                   90           18 USA U-18         
#> # ℹ 26 more variables: amateur_league <chr>, birth_date <chr>,
#> #   birth_place <chr>, country_code <chr>, cs_player_id <int>,
#> #   draft_date <chr>, draft_master_id <int>, draft_year <int>,
#> #   drafted_by_team_id <int>, first_name <chr>, height <int>, last_name <chr>,
#> #   notes <lgl>, overall_pick_number <int>, pick_in_round <int>,
#> #   player_id <int>, player_name <chr>, position <chr>, removed_outright <chr>,
#> #   removed_outright_why <lgl>, round_number <int>, shoots_catches <chr>, …
# }
```
