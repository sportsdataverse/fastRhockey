# NHL Expected Goals (xG) in fastRhockey: How the Model Works

**Expected goals (xG) is the probability that an unblocked shot becomes
a goal, given where it was taken from, how it was taken, and what
happened just before it.** A tip-in from the crease might carry an xG of
0.25; a wrist shot from the point, 0.02. Add xG up over a game or a
season and you get the number of goals an average shooter would have
scored on those chances. The gap between goals and xG separates
finishing and goaltending from shot generation.

This article explains the model behind the `xg` column in
`fastRhockey`’s NHL play-by-play: what it predicts, which features it
uses, how it was trained, how well it does, and where it is weakest.
Every number in a code block below is computed when the page is built.
Numbers from training are quoted from the [official model
card](https://github.com/sportsdataverse/fastRhockey-nhl-data/blob/main/docs/models/nhl_xg.md)
and labelled that way.

The PWHL has its own, different model. See the [PWHL expected goals
article](https://fastRhockey.sportsdataverse.org/articles/pwhl-expected-goals-xg-model.md).

**Changed in fastRhockey 1.1.0.** NHL `x_fixed` / `y_fixed` now rotate
the rink by the NHL API’s `homeTeamDefendingSide`, so the home team
always shoots right. Before 1.1.0 every home-team event got
`x_fixed = abs(x)` and every away-team event `-abs(x)`, which mirrored
any play from a team’s own half into its attacking half. That changes
`shot_distance`, `shot_angle` and the previous-event features, and so
xG, for NHL play-by-play. The models were retrained on 2026-11-13 on a
corpus built with the same rotation. The [coordinates
section](#how-are-the-coordinates-set) shows the result in the data.

## Which xG are you looking at?

There are two ways to get NHL xG from `fastRhockey`. They use the same
model files and, as shown
[below](#do-the-published-values-match-a-local-run), give the same
numbers.

|  | Published `xg` column | Computed locally |
|----|----|----|
| **How you get it** | [`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md), [`load_nhl_pbp_full()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp_full.md) | [`nhl_game_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_pbp.md) / [`nhl_game_feed()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_feed.md), or [`helper_nhl_calculate_xg()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_calculate_xg.md) on your own frame |
| **Model** | The boosters in the `nhl_xg_models` release (retrained 2026-11-13) | The same boosters, downloaded on first use, checked against the sha256s in the release manifest (`xg_model_meta.json`) and cached |
| **Needs `xgboost`?** | No | Yes. Without it [`nhl_game_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_pbp.md) and [`helper_nhl_calculate_xg()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_calculate_xg.md) return `xg = NA` and warn once per session (class `fastRhockey_no_xgboost`). |
| **When it changes** | When a season file is rescored after a retrain | On the first xG call of a session after a retrain: the cached files no longer match the release manifest, so they are replaced. [`refresh_xg_models()`](https://fastRhockey.sportsdataverse.org/reference/refresh_xg_models.md) forces a fresh download. |

Two practical notes on
[`helper_nhl_calculate_xg()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_calculate_xg.md):

- **A frame may hold several games.** Predictions are joined back by
  `game_id` and `event_id`, so the rows and their order are kept, and an
  existing `xg` column is replaced.
- The model files live in `tools::R_user_dir("fastRhockey", "cache")`.
  Each session re-reads the small release manifest; files that still
  match it are not downloaded again. Offline, the last manifest seen and
  its cached files are used.
  [`refresh_xg_models()`](https://fastRhockey.sportsdataverse.org/reference/refresh_xg_models.md)
  deletes the files and downloads the current versions.

## What does the model predict?

The model scores **unblocked shot attempts**, the events hockey analysts
call Fenwick events: `SHOT` (on goal), `MISSED_SHOT` and `GOAL`. Blocked
shots never reach the goalie, so they get no xG. Before scoring, the
feature builder
([`helper_nhl_prepare_xg_data()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_prepare_xg_data.md))
drops:

- shootout attempts (`period_type == "SHOOTOUT"`);
- penalty shots, which get a constant instead (below);
- line changes (`CHANGE`), so the “previous event” is always a hockey
  play;
- shots whose previous event is not one of `FACEOFF`, `GIVEAWAY`,
  `TAKEAWAY`, `BLOCKED_SHOT`, `HIT`, `MISSED_SHOT`, `SHOT`, `STOP`,
  `PENALTY` or `GOAL`.

The output is three components:

1.  **A 5v5 model**, for every shot where `strength_state == "5v5"`.
2.  **A special-teams model**, for every other strength state: power
    play, shorthanded, 4v4, 3v3 and empty-net situations such as 6v5.
3.  **A penalty-shot constant**, the historical conversion rate of
    penalty shots, assigned to every penalty shot.

Both models are XGBoost binary classifiers with a log-loss objective.
They follow the recipe of Daniel Morse’s
[hockeyR](https://github.com/danmorse314/hockeyR) xG model, re-fit on
the `fastRhockey` NHL corpus.

## Which features does it use?

All features are built in
[`helper_nhl_prepare_xg_data()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_prepare_xg_data.md)
from the play-by-play columns listed in
[`?nhl_xg`](https://fastRhockey.sportsdataverse.org/reference/nhl_xg.md).

| Group | Features | Notes |
|----|----|----|
| Shot geometry | `shot_distance`, `shot_angle` | Feet and degrees to the attacked net (at x = ±89 ft) |
| Shot type | `wrist_shot`, `snap_shot`, `slap_shot`, `backhand`, `tip_in`, `deflected`, `wrap_around`, `poke`, `batted`, `between_legs`, `cradle` | One-hot. The newer NHL API’s lowercase types (`wrist`, `tip-in`, `bat`, …) are mapped back to the older title-case names the model was trained on |
| Previous event | `last_faceoff`, `last_shot`, `last_missed_shot`, `last_blocked_shot`, `last_giveaway`, `last_takeaway`, `last_hit`, `last_stop`, `last_penalty`, `last_goal` | One-hot type of the event before the shot, within the same game and period |
| Pre-shot movement | `time_since_last`, `distance_from_last`, `last_x`, `last_y` | Seconds and feet since the previous event, and where it happened |
| Tactical flags | `rebound`, `rush`, `cross_ice_event` | Rebound: previous event an unblocked shot within 2 s. Rush: previous event in the neutral or defensive zone within 4 s. Cross-ice: previous event in the offensive zone on the other side of the ice (`y` crossing ±3 ft) within 2 s |
| Game state | `empty_net` | The shot was at an empty net |
| Era | `era_2011_2013`, `era_2014_2018`, `era_2019_2021`, `era_2022_2024`, `era_2025_on` | One-hot season buckets, so rule and tracking changes are modelled instead of averaged away |
| Special teams only | `total_skaters_on`, `event_team_advantage` | Skaters on the ice, and the shooting team’s skater edge |

You can read the exact feature lists out of the cached model metadata.

``` r

library(fastRhockey)
library(dplyr)
library(ggplot2)

xg_env <- fastRhockey:::.xg_env
# The models load on first use; load them here (a download the first time).
xg_ready <- requireNamespace("xgboost", quietly = TRUE) &&
  !inherits(try(fastRhockey:::.load_xg_models(), silent = TRUE), "try-error") &&
  !is.null(xg_env$xg_model_5v5)
xg_ready
#> [1] TRUE
```

``` r

xg_env$xg_feature_names_5v5
#>  [1] "shot_distance"      "shot_angle"         "rebound"           
#>  [4] "rush"               "time_since_last"    "distance_from_last"
#>  [7] "cross_ice_event"    "empty_net"          "last_x"            
#> [10] "last_y"             "era_2011_2013"      "era_2014_2018"     
#> [13] "era_2019_2021"      "era_2022_2024"      "era_2025_on"       
#> [16] "wrist_shot"         "snap_shot"          "slap_shot"         
#> [19] "backhand"           "wrap_around"        "tip_in"            
#> [22] "deflected"          "poke"               "batted"            
#> [25] "between_legs"       "cradle"             "last_faceoff"      
#> [28] "last_giveaway"      "last_takeaway"      "last_blocked_shot" 
#> [31] "last_hit"           "last_missed_shot"   "last_shot"         
#> [34] "last_stop"          "last_penalty"       "last_goal"

setdiff(xg_env$xg_feature_names_st, xg_env$xg_feature_names_5v5)
#> [1] "total_skaters_on"     "event_team_advantage"

# the penalty-shot constant
xg_env$xg_model_ps
#> [1] 0.3183639
```

### What does the 5v5 model lean on?

Gain is the share of the model’s total loss reduction that comes from
splits on each feature.

``` r

imp <- xgboost::xgb.importance(model = xg_env$xg_model_5v5)
head(imp[, c("Feature", "Gain")], 12)
#>                Feature       Gain
#>                 <char>      <num>
#>  1:      shot_distance 0.53588353
#>  2:         shot_angle 0.11350052
#>  3:    time_since_last 0.10324887
#>  4:        era_2025_on 0.03169979
#>  5: distance_from_last 0.02602005
#>  6:          snap_shot 0.01563065
#>  7:             tip_in 0.01499178
#>  8:             last_y 0.01394966
#>  9:            rebound 0.01256589
#> 10:           backhand 0.01211918
#> 11:             last_x 0.01197956
#> 12:          last_shot 0.01088873
```

``` r

imp %>%
  head(12) %>%
  ggplot(aes(x = Gain, y = reorder(Feature, Gain))) +
  geom_col(fill = "#531896") +
  labs(
    title = "NHL 5v5 xG model: feature importance (gain)",
    x = "Share of total gain", y = NULL,
    caption = "Data: fastRhockey xG model files"
  ) +
  theme_minimal()
```

![Horizontal bar chart of the twelve features with the highest gain in
the NHL 5v5 expected goals
model.](nhl-expected-goals-xg-model_files/figure-html/importance_plot-1.png)

Distance dominates, as in every public xG model. Empty net ranks high
because it is rare but decisive. Shot type and the time since the
previous event come next.

## How was it trained?

As reported in the [model
card](https://github.com/sportsdataverse/fastRhockey-nhl-data/blob/main/docs/models/nhl_xg.md)
and its model registry:

- **Data:** the NHL play-by-play corpus of the
  [fastRhockey-nhl-data](https://github.com/sportsdataverse/fastRhockey-nhl-data)
  repository, 18 seasons from 2009-10 to 2026-27 (2026-27 in progress at
  training time), grouped into the five era buckets above (2009-10 is
  the reference era). The corpus was rebuilt with the 1.1.0 rink
  rotation before the fit. The 5v5 model is fit on 5v5 shots only; the
  special-teams model on every other strength state.
- **Split:** an 80/20 train/holdout split grouped by `game_id` (no game
  appears on both sides), seed 37, with grouped 5-fold cross-validation
  on the training side.
- **Algorithm:** XGBoost, binary log-loss.
- **Fit in use:** retrained on 2026-11-13 (release manifest `version`
  2026-11-13).
- **Cross-validation:** 5v5 CV AUC **0.782** and CV log-loss **0.1906**;
  special teams CV AUC **0.7574** and CV log-loss **0.2806**.
- **Held-out games:** on the 20% of games kept out of training, AUC
  **0.7819** (5v5, log-loss 0.1908 against a 0.2213 constant baseline)
  and **0.7607** (special teams, log-loss 0.2772 against 0.3272). Per
  season on those games, goals divided by summed xG stays between about
  0.93 and 1.03 at 5v5.

## How well does it do on 2025-26?

Below, the published `xg` column is scored against the 2025-26 regular
season. That season is in the training corpus (about 80% of its games
were in the training split), so read this as a sanity check, not a
holdout.

``` r

pbp <- load_nhl_pbp(seasons = 2026)

shots <- pbp %>%
  filter(season_type == "R", !is.na(xg)) %>%
  mutate(
    goal = as.integer(event_type == "GOAL"),
    model = if_else(strength_state == "5v5", "5v5", "special teams")
  )

rank_auc <- function(y, p) {
  r <- rank(p)
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
log_loss <- function(y, p) {
  p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

eval_tbl <- shots %>%
  group_by(model) %>%
  summarise(
    shots = n(),
    goals = sum(goal),
    sum_xg = round(sum(xg), 1),
    goals_per_xg = round(goals / sum(xg), 3),
    log_loss = round(log_loss(goal, xg), 4),
    baseline_log_loss = round(log_loss(goal, rep(mean(goal), n())), 4),
    rank_auc = round(rank_auc(goal, xg), 4),
    .groups = "drop"
  )
eval_tbl
#> # A tibble: 2 × 8
#>   model      shots goals sum_xg goals_per_xg log_loss baseline_log_loss rank_auc
#>   <chr>      <int> <int>  <dbl>        <dbl>    <dbl>             <dbl>    <dbl>
#> 1 5v5        86220  5260  5289.        0.994    0.196             0.230    0.793
#> 2 special t… 23857  2625  2745.        0.956    0.290             0.347    0.766
```

On this season the 5v5 model ranks shots with an AUC of 0.7926 and beats
a constant-rate baseline on log-loss. Goals divided by xG is 0.994 at
5v5 and 0.956 on special teams; a value of 1 means summed xG matches
actual goals.

### Is it calibrated?

A calibrated model’s shots with xG near 0.10 go in about 10% of the
time. Here the shots are split into ten equal-sized bins by xG.

``` r

calib <- shots %>%
  group_by(model, decile = ntile(xg, 10)) %>%
  summarise(mean_xg = mean(xg), goal_rate = mean(goal), shots = n(), .groups = "drop")

ggplot(calib, aes(x = mean_xg, y = goal_rate, colour = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("5v5" = "#531896", "special teams" = "#368EAF")) +
  labs(
    title = "Calibration by xG decile, 2025-26 regular season",
    x = "Mean xG in decile", y = "Actual goal rate", colour = NULL,
    caption = "Data: fastRhockey::load_nhl_pbp()"
  ) +
  theme_minimal()
```

![Calibration plot of NHL expected goals by decile for 2025-26, actual
goal rate against mean xG, with a dashed
diagonal.](nhl-expected-goals-xg-model_files/figure-html/calibration-1.png)

### How does xG fall off with distance?

``` r

shots %>%
  filter(model == "5v5", !empty_net, shot_distance <= 90) %>%
  mutate(distance_bin = cut(shot_distance, breaks = seq(0, 90, 5))) %>%
  group_by(distance_bin) %>%
  summarise(`Mean xG` = mean(xg), `Goal rate` = mean(goal), .groups = "drop") %>%
  tidyr::pivot_longer(-distance_bin) %>%
  ggplot(aes(x = distance_bin, y = value, colour = name, group = name)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("Mean xG" = "#531896", "Goal rate" = "#37C4E3")) +
  labs(
    title = "5v5 xG and goal rate by shot distance, 2025-26",
    subtitle = "Non-empty-net unblocked shots",
    x = "Shot distance (ft)", y = NULL, colour = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![Line chart of mean expected goals and actual goal rate by shot
distance bin for 2025-26 5v5 NHL
shots.](nhl-expected-goals-xg-model_files/figure-html/distance_curve-1.png)

## Do the published values match a local run?

Yes. This recomputes xG for the first 30 regular-season games with
[`helper_nhl_calculate_xg()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_calculate_xg.md),
one game at a time, and compares it with the published column.

``` r

games <- sort(unique(pbp$game_id[pbp$season_type == "R"]))[1:30]

local_xg <- pbp %>%
  filter(game_id %in% games) %>%
  rename(xg_published = xg) %>%
  group_split(game_id) %>%
  lapply(function(g) helper_nhl_calculate_xg(as.data.frame(g))) %>%
  bind_rows()

local_xg %>%
  filter(!is.na(xg) | !is.na(xg_published)) %>%
  summarise(
    shots = n(),
    both_present = sum(!is.na(xg) & !is.na(xg_published)),
    max_abs_diff = max(abs(xg - xg_published), na.rm = TRUE)
  )
#>   shots both_present max_abs_diff
#> 1  2492         2492            0
```

A `max_abs_diff` of 0 means every shot with both a published and a local
`xg` matches exactly. Check that `both_present` equals `shots`:
`max_abs_diff` ignores shots that lack one of the two values. A larger
value means the season file was scored with a different set of models
from the one in your cache.

## How are the coordinates set?

`x_fixed` puts the home team shooting right. Shots should then sit
almost entirely at the attacked end (`x_fixed > 0` for the home team,
`< 0` for the away team), while hits and giveaways, which happen all
over the ice, split roughly evenly. A blocked shot’s event team is the
blocker, so it sits at the blocker’s own end.

``` r

pbp %>%
  filter(
    event_type %in% c("HIT", "BLOCKED_SHOT", "GIVEAWAY", "SHOT"),
    !is.na(x_fixed), x != 0
  ) %>%
  group_by(event_type, event_team_type) %>%
  summarise(
    events = n(),
    share_raw_x_positive = round(mean(x > 0), 3),
    share_x_fixed_positive = round(mean(x_fixed > 0), 3),
    .groups = "drop"
  )
#> # A tibble: 8 × 5
#>   event_type  event_team_type events share_raw_x_positive share_x_fixed_positive
#>   <chr>       <chr>            <int>                <dbl>                  <dbl>
#> 1 BLOCKED_SH… away             22131                0.484                  0.997
#> 2 BLOCKED_SH… home             21531                0.512                  0.003
#> 3 GIVEAWAY    away             21276                0.496                  0.569
#> 4 GIVEAWAY    home             21321                0.502                  0.451
#> 5 HIT         away             28597                0.505                  0.478
#> 6 HIT         home             30127                0.503                  0.542
#> 7 SHOT        away             34010                0.505                  0.032
#> 8 SHOT        home             35332                0.49                   0.971
```

Plays for which the feed gives no side use the period’s side, then where
each team’s shots cluster. `shot_distance` and `shot_angle` for a
`BLOCKED_SHOT` are measured to the shooter’s target net.

## Where are the estimates weakest?

- **Season-level drift.** The era indicators absorb most rule and
  tracking changes, and per-season calibration on held-out games stays
  within about 7% at 5v5, but a new season can drift before the next
  retrain (an annual July run in fastRhockey-nhl-data). Check
  `goals / sum(xg)` before treating xG totals as goal-scale.
- **No pre-shot passing data.** Public NHL feeds do not record passes or
  screens, so one-timers and royal-road passes are under-rated by
  construction.
- **Rink and scorer effects.** Shot locations are recorded by arena
  scorers. No rink adjustment is applied.
- **Small special-teams samples.** The special-teams model has a little
  over a quarter of the 5v5 training shots, so treat it as noisier.

## How do I reproduce this?

``` r

install.packages(c("fastRhockey", "xgboost"))
library(fastRhockey)

# published xG, a whole season
pbp <- load_nhl_pbp(seasons = 2026)

# live play-by-play for one game, with xG computed locally
game <- nhl_game_pbp(game_id = 2025020001)

# your own frame (one or many games)
pbp_xg <- helper_nhl_calculate_xg(pbp)

# force a fresh download of the cached model files
refresh_xg_models()
```

The training code, evaluation notebook and model files are in
[fastRhockey-nhl-data](https://github.com/sportsdataverse/fastRhockey-nhl-data).
The model card is
[docs/models/nhl_xg.md](https://github.com/sportsdataverse/fastRhockey-nhl-data/blob/main/docs/models/nhl_xg.md).

## Where to go next

- [PWHL expected goals (xG)
  model](https://fastRhockey.sportsdataverse.org/articles/pwhl-expected-goals-xg-model.md):
  the women’s professional league’s coordinate model.
- [Corsi, Fenwick and PDO in
  R](https://fastRhockey.sportsdataverse.org/articles/hockey-advanced-metrics-corsi-fenwick.md):
  shot-share metrics, including xGF%.
- [Rink plots and shot
  maps](https://fastRhockey.sportsdataverse.org/articles/nhl-pwhl-rink-plots-shot-maps.md):
  draw xG-sized shot maps.
- [Getting started with
  fastRhockey](https://fastRhockey.sportsdataverse.org/articles/introduction.md),
  and the [Helpers section of the
  reference](https://fastRhockey.sportsdataverse.org/reference/index.html#helpers)
  for
  [`helper_nhl_calculate_xg()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_calculate_xg.md),
  [`refresh_xg_models()`](https://fastRhockey.sportsdataverse.org/reference/refresh_xg_models.md)
  and
  [`?nhl_xg`](https://fastRhockey.sportsdataverse.org/reference/nhl_xg.md).

## Citation

Please cite the model as part of `fastRhockey`:

``` bibtex
@misc{howell_gilani_fastRhockey,
  author = {Ben Howell and Saiem Gilani},
  title = {fastRhockey: Access Hockey Play-by-Play Data and Statistics},
  url = {https://fastRhockey.sportsdataverse.org/},
  doi = {10.32614/CRAN.package.fastRhockey},
  year = {2026}
}
```

The model recipe follows Daniel Morse’s
[hockeyR](https://github.com/danmorse314/hockeyR) xG model.
