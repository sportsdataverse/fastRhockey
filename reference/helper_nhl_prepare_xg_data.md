# Prepare PBP data for xG model predictions

Helper function to prepare fastRhockey PBP data for expected goals (xG)
calculations.

Adapted from the hockeyR `helper_nhl_prepare_xg_data()` function. The
feature engineering logic (lag events, era dummies, shot metrics,
one-hot encoding) is identical to the hockeyR xG training pipeline so
that the same xgboost models can be used for inference.

## Usage

``` r
helper_nhl_prepare_xg_data(x)
```

## Arguments

- x:

  A play-by-play data frame produced by
  [`nhl_game_pbp`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_pbp.md)
  or
  [`nhl_game_feed`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_feed.md).

## Value

A tibble with model features plus identifiers (`season`, `game_id`,
`event_id`, `strength_state`).

## Details

The function:

1.  Filters out shootouts, penalty shots, and shift-change events.

2.  Computes lag features (last event type/team, time since last,
    distance from last, event zones).

3.  Adds era dummies for the four training eras (2011-13, 2014-18,
    2019-21, 2022+).

4.  Computes tactical booleans: `rebound`, `rush`, `cross_ice_event`.

5.  One-hot encodes `shot_type` and `last_event_type`.

6.  Pads any missing model features to zero so the xgboost DMatrix
    dimensions match the trained models.
