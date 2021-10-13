# whockey_scraper
Play-by-play scraper for the PHF (formerly known as the NWHL)

## load_raw_data
Loads in all the raw data for the game_id in a list format. Takes `game_id` as an input.

## process_period
Formats the raw data for a period into a workable format. Takes the raw data for a period as an input.

## process_shootout
Formats the raw data of a shootout into a workable format. Takes the raw data of the shootout as an input

## pbp_data
Takes the raw data from `load_raw_data` as an input then uses `process_period` and `process_shootout` to pull out all the by period data and then put it all into one pbp dataframe with 

## load_pbp
Pairs `load_raw_data` and `pbp_data` to pull the raw data and cleaning/set-up in one function that takes `game_id` as an input
