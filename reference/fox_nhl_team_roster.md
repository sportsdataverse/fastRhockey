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
#> ℹ Data updated: 2026-10-09 03:19:37 UTC
#> # A tibble: 22 × 9
#>    team_id position_group player      pos   age   ht    wt    college athlete_id
#>    <chr>   <chr>          <chr>       <chr> <chr> <chr> <chr> <chr>   <chr>     
#>  1 1       CENTER         Morgan Gee… C     28    "6'3… 212 … -       5576      
#>  2 1       CENTER         James Hage… C     19    "5'1… 186 … Boston… 8378      
#>  3 1       CENTER         Mark Kaste… C     27    "6'4… 231 … -       5732      
#>  4 1       CENTER         Marat Khus… C     24    "5'1… 189 … -       6400      
#>  5 1       CENTER         Sean Kuraly C     33    "6'2… 214 … Miami … 5053      
#>  6 1       CENTER         Elias Lind… C     31    "6'1… 204 … -       3619      
#>  7 1       CENTER         Fraser Min… C     22    "6'2… 207 … -       7178      
#>  8 1       CENTER         Casey Mitt… C     27    "6'1… 202 … Minnes… 5796      
#>  9 1       CENTER         Matthew Po… C     22    "6'0… 189 … -       7193      
#> 10 1       CENTER         Pavel Zacha C     29    "6'4… 212 … -       4746      
#> # ℹ 12 more rows
```
