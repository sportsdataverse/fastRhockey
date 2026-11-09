# **Get ESPN NHL Game Data (PBP + Team Box + Player Box)**

**Get ESPN NHL Game Data (PBP + Team Box + Player Box)**

## Usage

``` r
espn_nhl_game_all(game_id, ...)
```

## Arguments

- game_id:

  ESPN event identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A named list with three elements, all parsed from a **single** HTTP
request to the ESPN summary endpoint:

- **`plays`** - `fastRhockey_data` tibble, one row per play. See
  [`espn_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_pbp.md)
  for the full column reference.

- **`team_box`** - `fastRhockey_data` tibble, one row per team (2 rows).
  See
  [`espn_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_box.md)
  for the full column reference.

- **`player_box`** - `fastRhockey_data` tibble, one row per player. See
  [`espn_nhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_box.md)
  for the full column reference.

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  g <- try(espn_nhl_game_all(game_id = "401688263"))
  if (!inherits(g, "try-error")) {
    names(g)           # "plays" "team_box" "player_box"
    nrow(g$plays)
    nrow(g$team_box)
    nrow(g$player_box)
  }
#> [1] 38
# }
```
