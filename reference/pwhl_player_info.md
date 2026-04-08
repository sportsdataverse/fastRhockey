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

A data frame with player profile data including:

- `player_id` - Player ID.

- `first_name` - First name.

- `last_name` - Last name.

- `jersey_number` - Jersey number.

- `position` - Position (e.g., "F", "D", "G").

- `shoots` - Shooting hand.

- `catches` - Catching hand (goalies).

- `birthdate` - Date of birth.

- `height` - Height.

- `weight` - Weight.

- `hometown` - Hometown.

- `nationality` - Nationality.

- `team_id` - Current team ID.

- `team_name` - Current team name.

- `image_url` - Player headshot URL.

- `draft_info` - Draft information.

## Examples

``` r
# \donttest{
  try(pwhl_player_info(player_id = 28))
#> ── PWHL Player Info ─────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 03:01:42 UTC
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
