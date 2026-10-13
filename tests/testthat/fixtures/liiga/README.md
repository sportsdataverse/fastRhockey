# Liiga fixtures (`liiga.fi/api/v2`)

**Per-route captures** (`*.json` in this directory): copied verbatim from
`sdv-internal-refs/liiga/captures/` (refs commit `ee1b52e`, captured 2026-10-12). They are truncated
(every array cut to 3 elements, id-keyed maps to 3 keys); see that directory's README for the example
ids (season 2026, game 1 HPK-TPS, team `55786244:hpk`, player `21870028`) and the variant naming.

**Full bodies** (`full/`): fetched untruncated on 2026-10-12, keyless, 1.2 s apart. Dtypes and the
returns-schemas of the box score and the games list are derived from these, not from the 3-row captures,
because Liiga sends a rate as an integer when it is whole (`expectedGoals: 3`, `distance: 0`,
`hardestShot: 25`) and the truncated captures can hold only the integer form.

| File | URL | Notes |
|---|---|---|
| `full/games__stats__2026__1.json` | `https://liiga.fi/api/v2/games/stats/2026/1` | complete box score of game 1 of 2025-26 (223 KB) |
| `full/games__playoffs__2026__3games.json` | `https://liiga.fi/api/v2/games?tournament=playoffs&season=2026` | trimmed to 3 complete games (55080, 55081, 55116) of the 60; 55116's away `expectedGoals` is the integer `3` |

**fastRhockey copy.** The captures and `full/` bodies above are copied unchanged from sdv-py
`tests/fixtures/liiga/` (sdv-py main `79dd939371d245b0935362b491b03f50d63f8a8d`, #826); `spec_paths.json` (a Python test input) is left out.

**`parity/`** is the sdv-py oracle for `tests/testthat/test-liiga.R`: for each of the 43 routes (the 23
wrappers, plus the other 12 `player_stats` and 6 `team_stats` `data_type` tables) `<route>.csv` is the frame
sdv-py's `liiga_parsers` returns for the fixture the wrapper's example call reads, with the documented
`columns` and the request stamped as the runtime stamps it. Nulls are written `\N` and a raw CR as `<CR>`
(R's text-mode CSV read would drop it). `parity.json` holds each route's fixture, row count and the polars
dtype of every column in frame order (= sdv-py's column list).

## File names vs sdv-py

Some files are renamed so that every path stays under the 100-byte tarball limit (R CMD check portable file names). The sdv-py name is the old one with `from__to__tournament__bool__` removed from the file name.
