# SHL parity frames

`<case>.csv` is the sdv-py frame (sportsdataverse-py origin/main, #826) built by the shl parser
on the SAME body in the parent directory, passed `columns=_PARSER_COLUMNS[<endpoint>]` as the
generated wrapper does. Nulls are written as `<<NA>>`; a `list[int]` cell is its ids joined by
`;`. `_dtypes.csv` holds each column's sdv-py dtype (str, int, float, bool, list[int]).
`../schemas.csv` is sdv-py's `SHL.SCHEMAS` column list (endpoint, column, dtype).

Regenerate with sdv-py's `.venv` python and `PYTHONPATH` set to a `git archive` of origin/main
(never the live tree), parsing each fixture the way `tests/hockey/shl/test_shl.py` does.
Cases: one per wrapper, `stats_info__<module>` for the 34 modules, `stats_info__full_*`,
`pbp__full`, `boxscore__full`, `layout_info__directory`, `player_crosswalk`.
