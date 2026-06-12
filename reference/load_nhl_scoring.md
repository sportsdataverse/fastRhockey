# **Load fastRhockey NHL scoring summary**

Helper that loads multiple seasons of NHL goal-scoring event data from
the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

## Usage

``` r
load_nhl_scoring(
  seasons = most_recent_nhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2011.

- ...:

  Additional arguments passed to an underlying function.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A data frame (`fastRhockey_data`) with one row per goal and the
following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| situationCode | character | Strength/situation code for the goal. |
| eventId | integer | NHL event identifier. |
| strength | character | Strength state at which the goal was scored. |
| playerId | integer | Player id of the goal scorer. |
| firstName | list | Scorer first name (localized list). |
| lastName | list | Scorer last name (localized list). |
| name | list | Scorer display name (localized list). |
| teamAbbrev | list | Scoring team abbreviation (localized list). |
| headshot | character | URL to the scorer headshot image. |
| highlightClipSharingUrl | character | Shareable URL for the goal highlight clip. |
| highlightClip | numeric | Highlight clip identifier. |
| discreteClip | numeric | Discrete clip identifier. |
| goalsToDate | integer | Scorer goal total to date in the season. |
| awayScore | integer | Away team score after the goal. |
| homeScore | integer | Home team score after the goal. |
| leadingTeamAbbrev | list | Abbreviation of the leading team (localized list). |
| timeInPeriod | character | Time within the period the goal was scored. |
| shotType | character | Type of shot on the goal. |
| goalModifier | character | Goal modifier (e.g. empty-net, power-play). |
| assists | list | List of assisting players on the goal. |
| pptReplayUrl | character | URL to the power-play replay, if any. |
| homeTeamDefendingSide | character | Side of the ice the home team defended. |
| isHome | logical | Whether the scoring team is the home team. |
| period_number | integer | Period number the goal was scored in. |
| period_type | character | Period type (REG/OT/SO). |

## Examples

``` r
# \donttest{
  try(load_nhl_scoring(2026))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 8,682 × 47
#>    situationCode eventId strength playerId headshot       highlightClipSharing…¹
#>    <chr>           <int> <chr>       <int> <chr>          <chr>                 
#>  1 1551              258 ev        8483493 https://asset… https://nhl.com/video…
#>  2 1551              274 ev        8478421 https://asset… https://nhl.com/video…
#>  3 1451              366 pp        8477409 https://asset… https://nhl.com/video…
#>  4 1551              630 ev        8476882 https://asset… https://nhl.com/video…
#>  5 1551             1101 ev        8480003 https://asset… https://nhl.com/video…
#>  6 1551              481 ev        8479638 https://asset… https://nhl.com/video…
#>  7 1560             1123 ev        8479638 https://asset… https://nhl.com/video…
#>  8 1560              112 ev        8481481 https://asset… https://nhl.com/video…
#>  9 1551              562 ev        8480039 https://asset… https://nhl.com/video…
#> 10 1551              647 ev        8484258 https://asset… https://nhl.com/video…
#> # ℹ 8,672 more rows
#> # ℹ abbreviated name: ¹​highlightClipSharingUrl
#> # ℹ 41 more variables: highlightClipSharingUrlFr <chr>, highlightClip <dbl>,
#> #   highlightClipFr <dbl>, discreteClip <dbl>, discreteClipFr <dbl>,
#> #   goalsToDate <int>, awayScore <int>, homeScore <int>, timeInPeriod <chr>,
#> #   shotType <chr>, goalModifier <chr>, assists <chr>, pptReplayUrl <chr>,
#> #   homeTeamDefendingSide <chr>, isHome <lgl>, game_id <int>, …
# }
```
