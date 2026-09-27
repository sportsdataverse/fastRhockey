# **Get Fox Sports NHL team roster**

**Get Fox Sports NHL team roster**

## Usage

``` r
fox_nhl_team_roster(team_id)
```

## Arguments

- team_id:

  Fox Bifrost team id (e.g. `"1"`).

## Value

A `fastRhockey_data` tibble, one row per player (`team_id`,
`position_group`, `player`, ..., `athlete_id`).

## Examples

``` r
 try(fox_nhl_team_roster("1")) 
#> ── Fox Sports NHL roster ────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-27 04:35:47 UTC
#> # A tibble: 41 × 9
#>    team_id position_group player      pos   age   ht    wt    college athlete_id
#>    <chr>   <chr>          <chr>       <chr> <chr> <chr> <chr> <chr>   <chr>     
#>  1 1       CENTER         Riley Duran C     24    "6'2… 174 … Provid… 6539      
#>  2 1       CENTER         Michael Ey… C     30    "6'0… 195 … St. Cl… 5808      
#>  3 1       CENTER         Brendan Ga… C     32    "6'2… 222 … -       4202      
#>  4 1       CENTER         Morgan Gee… C     28    "6'3… 212 … -       5576      
#>  5 1       CENTER         James Hage… C     19    "5'1… 177 … Boston… 8378      
#>  6 1       CENTER         Mark Kaste… C     27    "6'4… 234 … -       5732      
#>  7 1       CENTER         Marat Khus… C     24    "5'1… 184 … -       6400      
#>  8 1       CENTER         Sean Kuraly C     33    "6'2… 208 … Miami … 5053      
#>  9 1       CENTER         Elias Lind… C     31    "6'1… 200 … -       3619      
#> 10 1       CENTER         Dans Locme… C     22    "6'0… 179 … -       8662      
#> # ℹ 31 more rows
```
