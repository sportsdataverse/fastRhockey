# **NHL Expected Goals (xG) Pipeline Overview**

Expected-goals computation for NHL play-by-play data. Three XGBoost
models (5v5, special teams, penalty shots) are downloaded on first use
from the sportsdataverse-data `nhl_xg_models` release (the one
sportsdataverse-py reads), checked against the sha256s in the release
manifest (`xg_model_meta.json`), and cached in
`tools::R_user_dir("fastRhockey", "cache")`. A cached copy the manifest
does not match is replaced, so a retrain reaches installed copies
without a package update, and
[`library(fastRhockey)`](https://fastRhockey.sportsdataverse.org/) makes
no network call.

## Details

### **Helpers**

|  |  |
|----|----|
| Function | Purpose |
| [`helper_nhl_prepare_xg_data()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_prepare_xg_data.md) | Build the model feature frame (event filtering, era dummies, skater counts, rebound / rush / cross-ice flags). (internal) |
| [`helper_nhl_calculate_xg()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_calculate_xg.md) | Run the three XGBoost predictions and append an `xg` column to the PBP frame. |

### **Era dummies**

The training corpus runs 2010-2024; the features keep one-hot era
indicators so the same models generalize past the training cutoff:

|                 |                              |
|-----------------|------------------------------|
| Indicator       | Seasons                      |
| `era_2011_2013` | 20102011, 20112012, 20122013 |
| `era_2014_2018` | 20132014 ... 20172018        |
| `era_2019_2021` | 20182019, 20192020, 20202021 |
| `era_2022_2024` | 20212022, 20222023, 20232024 |
| `era_2025_on`   | seasons after 20232024       |

### **Required PBP columns**

The input `pbp` data frame must include `event_type`, `secondary_type`,
`period_type`, `period`, `game_seconds`, `x`, `y`, `x_fixed`,
`event_team_abbr`, `home_abbr`, `away_abbr`, `home_skaters`,
`away_skaters`, `shot_distance`, `shot_angle`, `empty_net`,
`strength_state`, `event_id`, and `season`.
[`nhl_game_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_pbp.md)
/
[`nhl_game_feed()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_feed.md)
produce all of them.

### **Dependencies**

The `xgboost` suggested package is required. If it is absent,
[`helper_nhl_calculate_xg()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_calculate_xg.md)
warns and returns `pbp` with an `xg = NA_real_` column.
