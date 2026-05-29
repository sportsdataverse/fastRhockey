# **PWHL Player Search**

Search for PWHL players by name.

## Usage

``` r
pwhl_player_search(search_term)
```

## Arguments

- search_term:

  Character string to search for (e.g., a player name or partial name).

## Value

A data frame (`fastRhockey_data`) with the following columns:

|               |           |                                |
|---------------|-----------|--------------------------------|
| col_name      | types     | description                    |
| player_id     | numeric   | Unique player identifier.      |
| first_name    | character | Player first name.             |
| last_name     | character | Player last name.              |
| position      | character | Player position.               |
| team_id       | numeric   | Unique team identifier.        |
| person_id     | numeric   | Unique person identifier.      |
| team_name     | character | Most recent team name.         |
| team_code     | character | Most recent team abbreviation. |
| jersey_number | character | Jersey number.                 |
| shoots        | character | Shooting hand.                 |
| catches       | character | Catching hand (goalies).       |
| height        | character | Player height.                 |
| weight        | character | Player weight.                 |
| birthdate     | character | Date of birth.                 |
| image_url     | character | Player headshot URL.           |

## Examples

``` r
# \donttest{
  try(pwhl_player_search(search_term = "Poulin"))
#> ── PWHL Player Search ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 16:57:32 UTC
#> # A tibble: 2 × 15
#>   player_id first_name  last_name position team_id person_id team_name team_code
#>       <dbl> <chr>       <chr>     <chr>      <dbl>     <dbl> <chr>     <chr>    
#> 1        31 Marie-Phil… Poulin    F              3        33 Montréal… MTL      
#> 2       165 Maude       Poulin-L… D              1       200 Boston F… BOS      
#> # ℹ 7 more variables: jersey_number <chr>, shoots <chr>, catches <chr>,
#> #   height <chr>, weight <chr>, birthdate <chr>, image_url <chr>
# }
```
