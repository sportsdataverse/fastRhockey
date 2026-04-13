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

A data frame with matching players, or NULL if no results.

- `player_id` - Player ID.

- `first_name` - First name.

- `last_name` - Last name.

- `position` - Position.

- `team_id` - Team ID.

- `team_name` - Team name.

- `jersey_number` - Jersey number.

- `birthdate` - Date of birth.

- `image_url` - Player headshot URL.

## Examples

``` r
# \donttest{
  try(pwhl_player_search(search_term = "Poulin"))
#> ── PWHL Player Search ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:21 UTC
#> # A tibble: 2 × 15
#>   player_id first_name  last_name position team_id person_id team_name team_code
#>       <dbl> <chr>       <chr>     <chr>      <dbl>     <dbl> <chr>     <chr>    
#> 1        31 Marie-Phil… Poulin    F              3        33 Montréal… MTL      
#> 2       165 Maude       Poulin-L… D              1       200 Boston F… BOS      
#> # ℹ 7 more variables: jersey_number <chr>, shoots <chr>, catches <chr>,
#> #   height <chr>, weight <chr>, birthdate <chr>, image_url <chr>
# }
```
