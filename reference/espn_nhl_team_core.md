# **Get ESPN NHL Team Core (core-v2)**

**Get ESPN NHL Team Core (core-v2)**

## Usage

``` r
espn_nhl_team_core(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"4"` for Chicago
  Blackhawks).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row containing enriched team
metadata from the core-v2 endpoint, including `$ref` links to
sub-resources:

|                    |           |                                         |
|--------------------|-----------|-----------------------------------------|
| col_name           | types     | description                             |
| team_id            | character | ESPN team identifier (echoed from arg). |
| id                 | character | ESPN team identifier.                   |
| guid               | character | Team global unique identifier.          |
| uid                | character | Team uid.                               |
| alternate_id_sdr   | character | SDR alternate identifier.               |
| slug               | character | URL slug.                               |
| location           | character | Team city/location.                     |
| name               | character | Team mascot name.                       |
| nickname           | character | Team nickname.                          |
| abbreviation       | character | Team abbreviation.                      |
| display_name       | character | Full display name.                      |
| short_display_name | character | Short display name.                     |
| color              | character | Primary color hex.                      |
| alternate_color    | character | Alternate color hex.                    |
| is_active          | logical   | Whether the team is active.             |
| is_all_star        | logical   | Whether the team is an all-star team.   |
| logo               | character | Primary logo URL.                       |
| record_ref         | character | Ref URL for team record resource.       |
| athletes_ref       | character | Ref URL for team athletes resource.     |
| venue_ref          | character | Ref URL for team venue resource.        |
| groups_ref         | character | Ref URL for team groups resource.       |
| statistics_ref     | character | Ref URL for team statistics resource.   |
| leaders_ref        | character | Ref URL for team leaders resource.      |
| injuries_ref       | character | Ref URL for team injuries resource.     |
| awards_ref         | character | Ref URL for team awards resource.       |
| franchise_ref      | character | Ref URL for franchise resource.         |
| events_ref         | character | Ref URL for team events resource.       |
| transactions_ref   | character | Ref URL for team transactions resource. |
| coaches_ref        | character | Ref URL for team coaches resource.      |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_team_core(team_id = "4"))
#> ── NHL Team Core data from ESPN core-v2 ─────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:35:27 UTC
#> # A tibble: 1 × 29
#>   team_id id    guid        uid   alternate_id_sdr slug  location name  nickname
#>   <chr>   <chr> <chr>       <chr> <chr>            <chr> <chr>    <chr> <chr>   
#> 1 4       4     e1595aca-8… s:70… 2601             chic… Chicago  Blac… Blackha…
#> # ℹ 20 more variables: abbreviation <chr>, display_name <chr>,
#> #   short_display_name <chr>, color <chr>, alternate_color <chr>,
#> #   is_active <lgl>, is_all_star <lgl>, logo <chr>, record_ref <chr>,
#> #   athletes_ref <chr>, venue_ref <chr>, groups_ref <chr>,
#> #   statistics_ref <chr>, leaders_ref <chr>, injuries_ref <chr>,
#> #   awards_ref <chr>, franchise_ref <chr>, events_ref <chr>,
#> #   transactions_ref <chr>, coaches_ref <chr>
# }
```
