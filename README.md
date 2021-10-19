
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whockeyR

<!-- badges: start -->
<!-- [![Version-Number](https://img.shields.io/github/r-package/v/BenHowell71/whockeyR?label=whockeyR&logo=R&style=for-the-badge)](https://github.com/BenHowell71/whockeyR) -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/benhowell71/whockeyR/workflows/R-CMD-check/badge.svg)](https://github.com/benhowell71/whockeyR/actions)
<!-- badges: end -->

Play-by-play scraper for the PHF (formerly known as the NWHL)

------------------------------------------------------------------------

## Installation

You can install the released version of
[**`whockeyR`**](https://github.com/BenHowell71/whockeyR/) from
[GitHub](https://github.com/BenHowell71/whockeyR) with:

``` r
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("BenHowell71/whockeyR", dependencies = TRUE, update = TRUE)
```

If you would prefer the `devtools` installation:

``` r
# if you would prefer devtools installation
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
# Alternatively, using the devtools package:
devtools::install_github(repo = "BenHowell71/whockeyR")
```

## `phf_schedule.R`

Contains the code scraping the schedule and pertinent league information
for each season

## `pbp_functions.R`

Contains the code for all the below functions

## `data-raw/pbp_data.R`

Test files that shows how the functions work for pulling play-by-play
data for a regulation game, an overtime game, and an overtime + shootout
game.

------------------------------------------------------------------------

### `load_raw_data()`

Loads in all the raw data for the game\_id in a list format. Takes
`game_id` as an input.

------------------------------------------------------------------------

### `process_period()`

Formats the raw data for a period into a workable format. Takes the raw
data for a period as an input.

------------------------------------------------------------------------

### `process_shootout()`

Formats the raw data of a shootout into a workable format. Takes the raw
data of the shootout as an input

------------------------------------------------------------------------

### `pbp_data()`

Takes the raw data from `load_raw_data` as an input then uses
`process_period` and `process_shootout` to pull out all the by period
data and then put it all into one pbp dataframe with

------------------------------------------------------------------------

### `load_pbp()`

Pairs `load_raw_data` and `pbp_data` to pull the raw data and
cleaning/set-up in one function that takes `game_id` as an input

# Follow [SportsDataverse](https://twitter.com/sportsdataverse) on Twitter and star this repo

[![Twitter
Follow](https://img.shields.io/twitter/follow/sportsdataverse?color=blue&label=%40sportsdataverse&logo=twitter&style=for-the-badge)](https://twitter.com/sportsdataverse)

[![GitHub
stars](https://img.shields.io/github/stars/BenHowell71/whockeyR.svg?color=eee&logo=github&style=for-the-badge&label=Star%20whockeyR&maxAge=2592000)](https://github.com/BenHowell71/whockeyR/stargazers/)

# **Our Authors**

-   [Ben Howell](https://twitter.com/BenHowell71)  
    <a href="https://twitter.com/BenHowell71" target="blank"><img src="https://img.shields.io/twitter/follow/BenHowell71?color=blue&label=%40BenHowell71&logo=twitter&style=for-the-badge" alt="@BenHowell71" /></a>
    <a href="https://github.com/BenHowell71" target="blank"><img src="https://img.shields.io/github/followers/BenHowell71?color=eee&logo=Github&style=for-the-badge" alt="@BenHowell71" /></a>

# **Our Contributors (they’re awesome)**

-   [Saiem Gilani](https://twitter.com/saiemgilani)  
    <a href="https://twitter.com/saiemgilani" target="blank"><img src="https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge" alt="@saiemgilani" /></a>
    <a href="https://github.com/saiemgilani" target="blank"><img src="https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge" alt="@saiemgilani" /></a>

## **Citations**

To cite the [**`whockeyR`**](https://benhowell71.github.io/whockeyR/) R
package in publications, use:

BibTex Citation

``` bibtex
@misc{howell_whockeyR_2021,
  author = {Ben Howell},
  title = {whockeyR: whockeyR: The SportsDataverse's R Package for Women's Hockey Data.},
  url = {https://benhowell71.github.io/whockeyR/},
  year = {2021}
}
```
