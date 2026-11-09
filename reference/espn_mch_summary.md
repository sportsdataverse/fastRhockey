# **Get ESPN MCH / WCH Raw Game Summary**

**Get ESPN MCH / WCH Raw Game Summary**

## Usage

``` r
espn_mch_summary(event_id, ...)

espn_wch_summary(event_id, ...)
```

## Arguments

- event_id:

  ESPN event identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A nested R list representing the full parsed ESPN summary JSON. **This
function does NOT return a tibble.** It is intended for power users who
need fields not exposed by pbp, team_box, or player_box.

Top-level keys typically include: `boxscore`, `format`, `gameInfo`,
`leaders`, `seasonseries`, `broadcasts`, `pickcenter`, `odds`, `plays`,
`header`, `standings`, and others.

- `$boxscore$teams` - list of 2 team stat objects.

- `$boxscore$players` - list of 2 player stat group objects.

- `$plays` - list of play objects.

- `$header$season` - season year and type.

- `$header$competitions[[1]]$competitors` - home/away team + score.

Shape:

|  |  |  |
|----|----|----|
| col_name | type | description |
| (list) | list | Named list, not a data frame. Top-level keys are listed above. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_summary()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_summary.md)
(columns inherited); a collection may be empty.

`espn_wch_summary()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  s <- try(espn_mch_summary(event_id = "401717648"))
  if (!inherits(s, "try-error")) {
    names(s)
    length(s$plays)
  }
#> [1] 12
# }
# \donttest{
  s <- try(espn_wch_summary(event_id = "401762970"))
  if (!inherits(s, "try-error")) {
    names(s)
    length(s$plays)
  }
#> [1] 12
# }
```
