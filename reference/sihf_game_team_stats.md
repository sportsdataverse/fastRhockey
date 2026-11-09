# **SIHF Team stats of one game**

Team stats of one game: shots by period, faceoffs by zone, penalty
minutes, power play and penalty kill.

Reads the Swiss Ice Hockey Federation statistics API (`data.sihf.ch`,
keyless JSON). SIHF keys a season by its END year (2026 = 2025-26). An
unknown `season`, `league_id` or `phase_id` is never an error upstream
(SIHF silently answers another one), so every table answer is checked
against the body's own filter echo and refused when it differs. Every
request is spaced at least one second from the last one to the host.

## Usage

``` r
sihf_game_team_stats(game_id, language = "en")
```

## Arguments

- game_id:

  Required. SIHF game id (game_id in sihf_results), e.g. 20261105000325.

- language:

  Response language. Only part of the body is translated: status,
  position and penalty names stay German.

## Value

A `fastRhockey_data` tibble with 4 columns. A zero-row tibble with these
columns when the payload holds no rows.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id (numeric join key; the details link of sihf_results, the searchQuery of the game wrappers). |
| statistic | character | Statistic label as SIHF prints it (e.g., "SOG Total", "FO% 1P", "PPT"). |
| home_value | character | Home team's value, as a string (counts, percentages and mm:ss times share the column). |
| away_value | character | Away team's value, as a string. |

## Errors

- `fastRhockey_no_data`: SIHF answered 404 (unknown game), or ignored a
  requested season, league or phase and served another one.

- `fastRhockey_bad_request`: SIHF answered 400 / 422, or an argument is
  invalid.

- `fastRhockey_asset_fetch`: any other non-2xx answer or a dead
  connection after retries, an empty or unreadable body, a stub or
  redirect body, a table without its filter echo, a truncated table, or
  a `gameoverview` body for another game.

## See also

[family_sihf](https://fastRhockey.sportsdataverse.org/reference/family_sihf.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(sihf_game_team_stats(game_id = 20261105000325))
#> ── SIHF Game Team Stats Information from data.sihf.ch ───── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:24:08 UTC
#> # A tibble: 49 × 4
#>    game_id statistic home_value away_value
#>      <dbl> <chr>     <chr>      <chr>     
#>  1 2.03e13 SOG Total 36         22        
#>  2 2.03e13 SOG 1P    11         7         
#>  3 2.03e13 SOG 2P    16         8         
#>  4 2.03e13 SOG 3P    9          7         
#>  5 2.03e13 SHM Total 10         9         
#>  6 2.03e13 SHM 1P    4          1         
#>  7 2.03e13 SHM 2P    5          4         
#>  8 2.03e13 SHM 3P    1          4         
#>  9 2.03e13 SHP Total 1          1         
#> 10 2.03e13 SHP 1P    0          0         
#> # ℹ 39 more rows
# }
```
