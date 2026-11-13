# **Get ESPN NHL Game Team Leaders (core-v2)**

**Get ESPN NHL Game Team Leaders (core-v2)**

## Usage

``` r
espn_nhl_game_team_leaders(event_id, cid = event_id, team_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- team_id:

  ESPN team/competitor identifier. Use
  `espn_nhl_game_teams(event_id)$team_id` to find valid team ids for a
  game.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per (category, leader). For a
typical completed NHL game there are 6 stat categories (goals, assists,
points, plusMinus, penaltyMinutes, saves) with one leader each, yielding
~6 rows per team:

|                  |           |                                       |
|------------------|-----------|---------------------------------------|
| col_name         | types     | description                           |
| event_id         | character | ESPN event id (echoed from arg).      |
| cid              | character | Competition id (echoed from arg).     |
| team_id          | character | ESPN team id (echoed from arg).       |
| category_name    | character | Stat category name (e.g. "goals").    |
| category_display | character | Stat category display name.           |
| category_short   | character | Stat category short display name.     |
| category_abbr    | character | Stat category abbreviation.           |
| display_value    | character | Leader stat display value (e.g. "2"). |
| value            | numeric   | Leader stat numeric value.            |
| athlete_ref      | character | `$ref` URL for the leading athlete.   |
| athlete_id       | character | Athlete id parsed from the `$ref`.    |
| team_ref         | character | `$ref` URL for the athlete's team.    |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb  <- espn_nhl_scoreboard(dates = "20250110")
    eid <- sb$game_id[1]
    tms <- espn_nhl_game_teams(event_id = eid)
    espn_nhl_game_team_leaders(event_id = eid, team_id = tms$team_id[1])
  })
#> ── NHL Game Team Leaders data from ESPN core-v2 ─────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:34 UTC
#> # A tibble: 41 × 12
#>    event_id  cid       team_id category_name category_display category_short
#>    <chr>     <chr>     <chr>   <chr>         <chr>            <chr>         
#>  1 401688263 401688263 5       goals         Goals            Goals         
#>  2 401688263 401688263 5       goals         Goals            Goals         
#>  3 401688263 401688263 5       goals         Goals            Goals         
#>  4 401688263 401688263 5       goals         Goals            Goals         
#>  5 401688263 401688263 5       goals         Goals            Goals         
#>  6 401688263 401688263 5       assists       Assists          Assists       
#>  7 401688263 401688263 5       assists       Assists          Assists       
#>  8 401688263 401688263 5       assists       Assists          Assists       
#>  9 401688263 401688263 5       assists       Assists          Assists       
#> 10 401688263 401688263 5       assists       Assists          Assists       
#> # ℹ 31 more rows
#> # ℹ 6 more variables: category_abbr <chr>, display_value <chr>, value <dbl>,
#> #   athlete_ref <chr>, athlete_id <chr>, team_ref <chr>
# }
```
