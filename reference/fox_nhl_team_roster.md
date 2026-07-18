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
#> ℹ Data updated: 2026-07-18 18:46:35 UTC
#> # A tibble: 27 × 9
#>    team_id position_group player      pos   age   ht    wt    college athlete_id
#>    <chr>   <chr>          <chr>       <chr> <chr> <chr> <chr> <chr>   <chr>     
#>  1 1       CENTER         Michael Ey… C     29    "6'0… 195 … St. Cl… 5808      
#>  2 1       CENTER         Brendan Ga… C     32    "6'2… 222 … -       4202      
#>  3 1       CENTER         Morgan Gee… C     27    "6'3… 212 … -       5576      
#>  4 1       CENTER         James Hage… C     19    "5'1… 177 … Boston… 8378      
#>  5 1       CENTER         Mark Kaste… C     27    "6'4… 234 … -       5732      
#>  6 1       CENTER         Marat Khus… C     24    "5'1… 184 … -       6400      
#>  7 1       CENTER         Sean Kuraly C     33    "6'2… 208 … Miami … 5053      
#>  8 1       CENTER         Elias Lind… C     31    "6'1… 200 … -       3619      
#>  9 1       CENTER         Fraser Min… C     22    "6'2… 204 … -       7178      
#> 10 1       CENTER         Casey Mitt… C     27    "6'1… 205 … Minnes… 5796      
#> # ℹ 17 more rows
```
