# **Get ESPN NHL Injuries**

**Get ESPN NHL Injuries**

## Usage

``` r
espn_nhl_injuries(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per injured player:

|  |  |  |
|----|----|----|
| col_name | types | description |
| team_id | character | ESPN team identifier. |
| team_display_name | character | Team display name. |
| player_id | character | ESPN player identifier. |
| player_display_name | character | Player display name. |
| player_short_name | character | Player short name. |
| player_position | character | Player position abbreviation. |
| injury_id | character | ESPN injury identifier. |
| injury_status | character | Injury status (e.g. "Questionable", "Out"). |
| injury_date | character | Injury date (ISO 8601). |
| injury_short_comment | character | Short injury comment. |
| injury_long_comment | character | Long injury comment. |
| injury_type | character | Injury type name. |
| injury_abbreviation | character | Injury type abbreviation. |
| detail_side | character | Injured side (e.g. "Left", "Right"). |
| detail_fantasy_status | character | Fantasy relevance status. |
| detail_return_date | character | Projected return date. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_injuries())
#> ── NHL Injuries data from ESPN.com ──────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:36 UTC
#> # A tibble: 218 × 16
#>    team_id team_display_name player_id player_display_name player_short_name
#>    <chr>   <chr>             <chr>     <chr>               <chr>            
#>  1 25      Anaheim Ducks     NA        Ryan Poehling       R. Poehling      
#>  2 25      Anaheim Ducks     NA        Ryan Poehling       R. Poehling      
#>  3 25      Anaheim Ducks     NA        Ian Moore           I. Moore         
#>  4 25      Anaheim Ducks     NA        Ian Moore           I. Moore         
#>  5 25      Anaheim Ducks     NA        A.J. Greer          A.J. Greer       
#>  6 25      Anaheim Ducks     NA        A.J. Greer          A.J. Greer       
#>  7 1       Boston Bruins     NA        James Hagens        J. Hagens        
#>  8 1       Boston Bruins     NA        James Hagens        J. Hagens        
#>  9 2       Buffalo Sabres    NA        Konsta Helenius     K. Helenius      
#> 10 2       Buffalo Sabres    NA        Konsta Helenius     K. Helenius      
#> # ℹ 208 more rows
#> # ℹ 11 more variables: player_position <chr>, injury_id <chr>,
#> #   injury_status <chr>, injury_date <chr>, injury_short_comment <chr>,
#> #   injury_long_comment <chr>, injury_type <chr>, injury_abbreviation <chr>,
#> #   detail_side <chr>, detail_fantasy_status <chr>, detail_return_date <chr>
# }
```
