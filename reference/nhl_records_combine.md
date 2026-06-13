# **NHL Records - Draft Combine Measurements**

Returns NHL Scouting Combine measurements from the NHL Records API
(`https://records.nhl.com/site/api/combine`).

## Usage

``` r
nhl_records_combine(cayenne_exp = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                   |           |                                       |
|-------------------|-----------|---------------------------------------|
| col_name          | types     | description                           |
| id                | integer   | Unique combine record identifier.     |
| amateur_club_name | character | Amateur club the player played for.   |
| amateur_league    | character | Amateur league the player played in.  |
| draft_year        | integer   | Draft year for the player.            |
| event             | character | Combine measurement event name.       |
| first_name        | character | Player first name.                    |
| last_name         | character | Player last name.                     |
| player_id         | integer   | Unique player identifier.             |
| position_code     | character | Player position code.                 |
| value             | numeric   | Measured value for the combine event. |

## Examples

``` r
# \donttest{
  try(nhl_records_combine())
#> ── NHL Records Combine ──────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-13 02:49:36 UTC
#> # A tibble: 185 × 10
#>       id amateur_club_name  amateur_league draft_year event first_name last_name
#>    <int> <chr>              <chr>               <int> <chr> <chr>      <chr>    
#>  1     1 Poprad             SLOVAKIA             2022 Benc… Filip      Mesar    
#>  2     2 St. Sebastian’s    HIGH-MA              2019 Benc… Jayden     Struble  
#>  3     3 Avon Old Farms     HIGH-CT              2022 Benc… Brennan    Ali      
#>  4     4 North Bay          OHL                  2022 Benc… Liam       Arnsby   
#>  5     5 USA U-18           NTDP                 2022 Benc… Seamus     Casey    
#>  6     6 Guelph             OHL                  2022 Benc… Jake       Karabela 
#>  7     7 Barrie             OHL                  2024 Benc… Cole       Beaudoin 
#>  8     8 Northeastern Univ. H-EAST               2022 Benc… Jack       Hughes   
#>  9     9 Skelleftea Jr.     SWEDEN-JR            2023 Benc… Axel       Sandin-P…
#> 10    10 Sarnia             OHL                  2024 Benc… Lukas      Fischer  
#> # ℹ 175 more rows
#> # ℹ 3 more variables: player_id <int>, position_code <chr>, value <dbl>
# }
```
