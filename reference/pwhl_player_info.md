# **PWHL Player Info**

Retrieves biographical and profile information for a PWHL player.

## Usage

``` r
pwhl_player_info(player_id)
```

## Arguments

- player_id:

  Numeric player ID

## Value

A data frame (`fastRhockey_data`) with the following columns:

|               |           |                                 |
|---------------|-----------|---------------------------------|
| col_name      | types     | description                     |
| player_id     | numeric   | Unique player identifier.       |
| first_name    | character | Player first name.              |
| last_name     | character | Player last name.               |
| name          | character | Player full name.               |
| jersey_number | character | Jersey number.                  |
| position      | character | Player position.                |
| shoots        | character | Shooting hand.                  |
| catches       | character | Catching hand (goalies).        |
| birthdate     | character | Date of birth.                  |
| height        | character | Player height.                  |
| weight        | character | Player weight.                  |
| birthtown     | character | Town of birth.                  |
| birthprov     | character | Province/state of birth.        |
| birthcntry    | character | Country of birth.               |
| nationality   | character | Player nationality.             |
| team_id       | numeric   | Current team unique identifier. |
| team_name     | character | Current team name.              |
| team_code     | character | Current team abbreviation.      |
| image_url     | character | Player headshot URL.            |
| draft_info    | character | Draft information.              |

## Examples

``` r
# \donttest{
  try(pwhl_player_info(player_id = 28))
#> ── PWHL Player Info ─────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-24 02:06:59 UTC
#> # A tibble: 1 × 20
#>   player_id first_name last_name name      jersey_number position shoots catches
#>       <dbl> <chr>      <chr>     <chr>     <chr>         <chr>    <chr>  <chr>  
#> 1        28 Ann-Renée  Desbiens  Ann-René… 35            G        L      L      
#> # ℹ 12 more variables: birthdate <chr>, height <chr>, weight <chr>,
#> #   birthtown <chr>, birthprov <chr>, birthcntry <chr>, nationality <chr>,
#> #   team_id <dbl>, team_name <chr>, team_code <chr>, image_url <chr>,
#> #   draft_info <chr>
# }
```
