# Calculate expected goals (xG) for play-by-play data

Adds an `xg` column to a play-by-play data frame using the fastRhockey
xG models (XGBoost).

The models were trained on NHL PBP data from 2010-2024 using the same
methodology as the [hockeyR](https://github.com/danmorse314/hockeyR)
(Daniel Morse) xG models. Three models are used:

- **5v5 model** - Even-strength situations

- **Special teams model** - Power play, short-handed, and other non-5v5
  situations (adds skater count features)

- **Penalty shot model** - A constant xG value for penalty shot attempts

The models are downloaded on the first call from the
sportsdataverse-data `nhl_xg_models` release, checked against the sha256
this version pins and cached, so later calls (and offline sessions with
a valid cache) make no network request. A cached copy from another
training is replaced.

## Usage

``` r
helper_nhl_calculate_xg(pbp)
```

## Arguments

- pbp:

  A play-by-play data frame produced by
  [nhl_game_pbp](https://fastRhockey.sportsdataverse.org/reference/nhl_game_pbp.md)
  or
  [nhl_game_feed](https://fastRhockey.sportsdataverse.org/reference/nhl_game_feed.md).
  Must contain columns needed by
  [`helper_nhl_prepare_xg_data()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_prepare_xg_data.md):
  `event_type`, `secondary_type`, `period_type`, `period`,
  `game_seconds`, `x`, `y`, `x_fixed`, `event_team_abbr`, `home_abbr`,
  `away_abbr`, `home_skaters`, `away_skaters`, `shot_distance`,
  `shot_angle`, `empty_net`, `strength_state`, `game_id`, `event_id`,
  `season`.

  `pbp` may hold more than one game: predictions are matched back by
  `game_id` and `event_id`. An existing `xg` column is replaced.

## Value

The original `pbp` data frame, same rows in the same order, with an `xg`
column appended (replacing any existing one):

|  |  |  |
|----|----|----|
| col_name | types | description |
| xg | numeric | Expected-goal probability of the shot, NA for non-shot events. |

If `xgboost` is not installed, the function raises a warning of class
`fastRhockey_no_xgboost` and every `xg` is NA.

## Examples

``` r
if (FALSE) { # \dontrun{
  pbp <- nhl_game_pbp(2024020001, include_shifts = TRUE)
  pbp <- helper_nhl_calculate_xg(pbp)
  # View xG for shots
  pbp[!is.na(pbp$xg), c("event_type", "shot_distance", "shot_angle", "xg")]
} # }
```
