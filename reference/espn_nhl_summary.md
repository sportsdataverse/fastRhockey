# **Get ESPN NHL Raw Game Summary**

**Get ESPN NHL Raw Game Summary**

## Usage

``` r
espn_nhl_summary(game_id, ...)
```

## Arguments

- game_id:

  ESPN event identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A nested R list representing the full parsed ESPN summary JSON. **This
function does NOT return a tibble.** It is intended for power users who
need fields not exposed by
[`espn_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_pbp.md),
[`espn_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_box.md),
or
[`espn_nhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_box.md).

Top-level keys typically include: `boxscore`, `format`, `gameInfo`,
`leaders`, `seasonseries`, `broadcasts`, `pickcenter`, `odds`, `plays`,
`header`, `standings`, and others.

- `$boxscore$teams` - list of 2 team stat objects.

- `$boxscore$players` - list of 2 player stat group objects.

- `$plays` - list of play objects.

- `$header$season` - season year and type.

- `$header$competitions[[1]]$competitors` - home/away team + score.

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  s <- try(espn_nhl_summary(game_id = "401688263"))
  if (!inherits(s, "try-error")) {
    names(s)
    length(s$plays)
  }
#> [1] 244
# }
```
