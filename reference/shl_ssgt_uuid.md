# SHL ssgt uuid of a season and game type

The SHL keys a season by an "ssgt" uuid (season + series + game type).
This maps a season (END year) and game type to it through
`/statistics-v2/layout-info`, fetched once per R session. Use it for the
routes that take an ssgt uuid directly
([`shl_standings()`](https://fastRhockey.sportsdataverse.org/reference/shl_standings.md),
[`shl_sites()`](https://fastRhockey.sportsdataverse.org/reference/shl_sites.md),
[`shl_ssgt_provider()`](https://fastRhockey.sportsdataverse.org/reference/shl_ssgt_provider.md),
[`shl_post_game_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/shl_post_game_team_stats.md),
[`shl_player_h2h()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_h2h.md)).

## Usage

``` r
shl_ssgt_uuid(season, game_type = "regular")
```

## Arguments

- season:

  Season END year (2026 = 2025/26).

- game_type:

  Game type: `"regular"` (Grundserie), `"playoff"` (Slutspel),
  `"qualdown"` (relegation qualification) or `"playin"` (2023/24 on).

## Value

A character string, the ssgt uuid (`"iuzqg7dqk9"` for the 2025/26
regular season). A length-one vector, not a data frame:

|          |           |                |
|----------|-----------|----------------|
| col_name | type      | description    |
| (value)  | character | The ssgt uuid. |

## Errors

`fastRhockey_no_data`: layout-info lists no ssgt for that season and
game type. `fastRhockey_bad_request`: `season` is not a year.
`fastRhockey_asset_fetch`: layout-info could not be fetched or read.

## See also

[family_shl](https://fastRhockey.sportsdataverse.org/reference/family_shl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(shl_ssgt_uuid(2026))
#> [1] "iuzqg7dqk9"
  try(shl_ssgt_uuid(2025, "playoff"))
#> [1] "fg2i6uu9dt"
# }
```
