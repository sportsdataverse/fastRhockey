# HockeyTech test fixtures

Real HockeyTech / LeagueStat replies (JSONP wrapper stripped). API keys are replaced by
`REDACTED`; array trim markers from the source captures are dropped.

| File | View | Source |
|---|---|---|
| `pwhl_schedule_8.json` | `modulekit/schedule&season_id=8` | All 120 games of the PWHL 2025-26 regular season, live 2026-10-08 (same file as sdv-py `tests/fixtures/hockeytech/pwhl_schedule_8.json`) |
| `pwhl_schedule_2025.json` | `modulekit/scorebar` | Sent `season_id`; holds 200 rows across 6 seasons, because scorebar ignores `season_id` |
| `pwhl_seasons.json` | `modulekit/seasons` | PWHL ids 1-10, ending at the 2026-27 preseason (sdv-py fixture) |
| `ahl_seasons.json` | `modulekit/seasons` | AHL, newest 25 of 76 seasons, sdv-internal-refs `hockeytech/captures/samples/ahl/seasons.json` (live 2026-07-12; same as sdv-py's fixture) |
| `ohl_seasons.json`, `whl_seasons.json`, `qmjhl_seasons.json` | `modulekit/seasons` | Newest 25 seasons each, sdv-internal-refs `hockeytech/captures/samples/<league>/seasons.json` (live 2026-10-08) |
| `seasons_parity_sdvpy.csv` | — | sdv-py `parse_seasons()` (`sportsdataverse-py` branch `fix/hockeytech-season-schedule`, commit e8dad8c) on the five `*_seasons.json` above: `season_id`, `season_name`, `season_yr`, `game_type_label`. The parity test asserts R reads every row the same way; regenerate it when sdv-py's season parsing changes. |
| other `pwhl_*.json` | per name | Shared with sdv-py `tests/fixtures/hockeytech/` (game 42, season 5) |
