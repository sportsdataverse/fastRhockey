
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastRhockey

<!-- badges: start -->
<!-- [![Version-Number](https://img.shields.io/github/r-package/v/BenHowell71/fastRhockey?label=fastRhockey&logo=R&style=for-the-badge)](https://github.com/BenHowell71/fastRhockey) -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/benhowell71/fastRhockey/workflows/R-CMD-check/badge.svg)](https://github.com/benhowell71/fastRhockey/actions)
<!-- badges: end -->

[`fastRhockey`](https://github.com/benhowell71/fastRhockey) is an R
Package that is designed to pull play-by-play (and boxscore) data from
the newest version of the [Premier Hockey Federation (PHF)
website](https://www.premierhockeyfederation.com/). In the past, there
have been a few scrapers for the PHF (formerly the NWHL), but they’ve
all been deprecated since the league changed website formats.

With the seventh season of the league kicking off on November 6th, and
games being broadcasted on ESPN+, this package was created to allow
access to play-by-play data to continue pushing women’s hockey analytics
forward.

In Spring of 2021, the [Big Data
Cup](https://www.theicegarden.com/2021/4/15/22374981/a-directory-of-womens-hockey-projects-from-big-data-cup-2021-analytics-otthac-stathletes)
and the [data they made
available](https://github.com/bigdatacup/Big-Data-Cup-2021)
revolutionized what we were able to thanks to the detailed play-by-play
data for the season and the x/y location data. That wave continued with
the inaugural [WHKYHAC conference](https://www.whkyhac.com/) in July
that produced some amazing conversations and projects in the women’s
hockey space.

In the past, the lack of data and poor access to data have been the
biggest barrier to entry in women’s hockey analytics, a barrier that
this package is intended to alleviate.

------------------------------------------------------------------------

## Installation

You can install the released version of
[**`fastRhockey`**](https://github.com/BenHowell71/fastRhockey/) from
[GitHub](https://github.com/BenHowell71/fastRhockey) with:

``` r
# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("BenHowell71/fastRhockey", dependencies = TRUE, update = TRUE)
```

If you would prefer the `devtools` installation:

``` r
# if you would prefer devtools installation
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
# Alternatively, using the devtools package:
devtools::install_github(repo = "BenHowell71/fastRhockey")
```

------------------------------------------------------------------------

Once the package has been installed, there’s a ton of stuff you can do.
Let’s start by finding a game we’re interested in, say, the 2021 Isobel
Cup Championship that the Boston Pride won.

``` r
# input the season that you're interested in looking up the schedule for
phf_schedule(season = 2021) %>%
  filter(game_type == "Playoffs") %>%
  filter(home_team_short == "MIN" & away_team_short == "BOS") %>%
  dplyr::select(game_id,
                date_group, facility,
                game_type, 
                home_team, away_team,
                home_score, away_score,
                winner)
#>   game_id date_group          facility game_type           home_team
#> 1  379254 2021-03-27 Warrior Ice Arena  Playoffs Minnesota Whitecaps
#>      away_team home_score away_score       winner
#> 1 Boston Pride          3          4 Boston Pride
```

A couple of quick filters/selects later and we’ve pared down the data
into a very manageable return. We can see that the Boston Pride beat the
Minnesota Whitecaps 4-3 in Warrior Ice Arena on March 27th, 2021. The
other important column in this return is the `game_id` column.

Let’s take that `game_id` and plug it into another `fastRhockey`
function, this time using the `load_boxscore` function to pull the
boxscore data from this game.

``` r
x <- 379254

box <- load_boxscore(game_id = x)
#> No encoding supplied: defaulting to UTF-8.

box %>%
  dplyr::select(game_id, 
                team, 
                total_scoring, total_shots,
                successful_power_play, power_play_opportunities,
                faceoff_percent, takeaways)
#> # A tibble: 2 x 8
#>   game_id team  total_scoring total_shots successful_power_play power_play_oppo~
#>     <dbl> <chr>         <int>       <int>                 <dbl>            <dbl>
#> 1  379254 bos               4          30                     2                3
#> 2  379254 min               3          30                     1                2
#> # ... with 2 more variables: faceoff_percent <dbl>, takeaways <dbl>
```

Once again, I’ve selected some specific columns, but this is an example
of the data that is returned by the `load_boxscore` function! We have
counting stat data on shots/goals, both aggregated and by period, power
play data, faceoff data, and how often a team takes/gives away the puck.
It’s definitely helpful data and I believe that there are some really
fun projects that can be done with just the `load_boxscore` function,
but the really good stuff is still coming.

Turn your attention to `load_pbp`, the function that was created to
return PHF play-by-play data for a given game (i.e. the whole reason
that `fastRhockey` exists). It’s a similar format to the boxscore
function where the only input necessary is the `game_id` that you want.

``` r
a <- Sys.time()

pbp <- load_pbp(game_id = x)
  
Sys.time() - a
#> Time difference of 7.228843 secs
```

Loading a single game should take \~ 5 seconds. Once it does, it’s time
to have some fun. The `load_pbp` function returns 34 columns, some with
“boring” data, like who the teams are, etc. But then you get to the
columns that look at how much time is remaing in a quarter, what the
home skater vs away skater numbers are, what event occured, who was
involved, and so on.

``` r
pbp
#> # A tibble: 204 x 36
#>    game_id home_team  away_team period_id event_no description    time_remaining
#>      <dbl> <chr>      <chr>         <dbl>    <int> <chr>          <chr>         
#>  1  379254 Minnesota~ Boston P~         1        1 #14 Jillian D~ 19:59         
#>  2  379254 Minnesota~ Boston P~         1        2 #17 McKenna B~ 19:43         
#>  3  379254 Minnesota~ Boston P~         1        3 #17 McKenna B~ 19:36         
#>  4  379254 Minnesota~ Boston P~         1        4 #21 Audra Mor~ 18:25         
#>  5  379254 Minnesota~ Boston P~         1        5 #21 Audra Mor~ 18:13         
#>  6  379254 Minnesota~ Boston P~         1        6 #21 Audra Mor~ 17:38         
#>  7  379254 Minnesota~ Boston P~         1        7 #9 Allie Thun~ 17:24         
#>  8  379254 Minnesota~ Boston P~         1        8 #23 Carlee Tu~ 16:54         
#>  9  379254 Minnesota~ Boston P~         1        9 #21 Christina~ 16:48         
#> 10  379254 Minnesota~ Boston P~         1       10 #19 Haley Mac~ 16:29         
#> # ... with 194 more rows, and 29 more variables: sec_from_start <dbl>,
#> #   on_ice_situation <chr>, home_skaters <dbl>, away_skaters <dbl>,
#> #   home_goals <chr>, away_goals <chr>, leader <chr>, team <chr>, event <chr>,
#> #   first_player <chr>, first_number <chr>, second_player <chr>,
#> #   second_number <chr>, third_player <chr>, third_number <chr>,
#> #   shot_type <chr>, shot_result <chr>, goalie_involved <chr>, penalty <dbl>,
#> #   penalty_length <chr>, penalty_called <chr>, offensive_player_one <chr>, ...
```

There’s data on who took a shot (if a shot occurs), as well as who the
primary (and secondary) assisters were and who the goalie was. Penalties
are recorded + the time assigned for a trip to the box.

One of the more interesting findings from the PHF set-up was that they
ID all five offensive players on the ice when a goal is scored, so
that’s available as well. Unfortunately, it’s hard to derive any sort of
plus/minus stat from this since it’s only the offensive players at the
time of a goal. If the offensive and defensive lineups were provided we
could create a +/-, but that remains out of reach for now.

Here’s an example of the things that one can now build with the
play-by-play data that is generated from `load_pbp`. This is a quick
graph showing cumulative shot attempts by point in the game for Boston
and Minnesota.

``` r
pbp %>%
  mutate(shot = ifelse(event %in% c("PP Goal", "Goal",
                                    "Pen Shot", "Shot", 
                                    "Shot BLK"), 1, 0)) %>%
  group_by(team) %>%
  mutate(total_shots = cumsum(shot)) %>%
  ggplot() +
  geom_line(aes(x = sec_from_start, y = total_shots, color = team),
            size = 2) +
  scale_color_manual(values = c("Boston PrideBOS" = "#b18c1e",
                                "Minnesota WhitecapsMIN" = "#1c449c")) +
  labs(y = "Total Shots",
       title = "Boston Pride vs Minnesota Whitecaps - 3/27/2021",
       subtitle = "Total Shots by Minute of Game") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 11),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 12),
    legend.title = element_blank()
  ) +
  scale_x_continuous(breaks = c(1200, 2400, 3600, 3800),
                     labels = c("End 1st", "End 2nd", "End 3rd", " ")) +
  scale_y_continuous(limits = c(0, 40))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

It’s a simple graph, but one that can easily help illustrate game flow.
The Pride’s shots came in bunches, taking a ton about halfway through
the first and third periods respectively. Minnesota started the game
slowly, but their shots came fairly consistently throughout the game.

There’s so much more that can be explored from this play-by-play data,
whether you want to explore how winning a faceoff leads to a shot
attempt or the chaos that can follow giveaways.

That’s a quick primer on the main functions of the package.
`phf_schedule` returns schedule information and game\_ids, which can be
used in `load_boxscore` or `load_pbp` to return boxscore or play-by-play
data. `load_game` wraps the boxscore/play-by-play functions into one and
returns a list with the two data frames.

The last function that may be of some use is `phf_league_info`, which
essentially pulls a lot of background info on the league and the IDs
that are used. The output from this function gets wrapped into the
`load___` functions and `phf_schedule`, which is it’s main purpose.

If you look with `fastRhockey::`, there are more functions available,
but those are helper functions to pull raw data (`phf_game_data`) and
then to process the raw data into a usable format (`process____`).

------------------------------------------------------------------------

# Follow [SportsDataverse](https://twitter.com/sportsdataverse) on Twitter and star this repo

[![Twitter
Follow](https://img.shields.io/twitter/follow/sportsdataverse?color=blue&label=%40sportsdataverse&logo=twitter&style=for-the-badge)](https://twitter.com/sportsdataverse)

[![GitHub
stars](https://img.shields.io/github/stars/BenHowell71/fastRhockey.svg?color=eee&logo=github&style=for-the-badge&label=Star%20fastRhockey&maxAge=2592000)](https://github.com/BenHowell71/fastRhockey/stargazers/)

# **Our Authors**

-   [Ben Howell](https://twitter.com/BenHowell71)  
    <a href="https://twitter.com/BenHowell71" target="blank"><img src="https://img.shields.io/twitter/follow/BenHowell71?color=blue&label=%40BenHowell71&logo=twitter&style=for-the-badge" alt="@BenHowell71" /></a>
    <a href="https://github.com/BenHowell71" target="blank"><img src="https://img.shields.io/github/followers/BenHowell71?color=eee&logo=Github&style=for-the-badge" alt="@BenHowell71" /></a>

# **Our Contributors (they’re awesome)**

-   [Saiem Gilani](https://twitter.com/saiemgilani)  
    <a href="https://twitter.com/saiemgilani" target="blank"><img src="https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge" alt="@saiemgilani" /></a>
    <a href="https://github.com/saiemgilani" target="blank"><img src="https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge" alt="@saiemgilani" /></a>  
-   [Alyssa Longmuir](https://twitter.com/alyssastweeting)  
    <a href="https://twitter.com/alyssastweeting" target="blank"><img src="https://img.shields.io/twitter/follow/alyssastweeting?color=blue&label=%40alyssastweeting&logo=twitter&style=for-the-badge" alt="@alyssastweeting" /></a>
    <a href="https://github.com/Aklongmuir" target="blank"><img src="https://img.shields.io/github/followers/Aklongmuir?color=eee&logo=Github&style=for-the-badge" alt="@Aklongmuir" /></a>
-   [Tan Ho](https://twitter.com/_TanHo)  
    <a href="https://twitter.com/_TanHo" target="blank"></a>
    <a href="https://github.com/tanho63" target="blank"><img src="https://img.shields.io/github/followers/tanho63?color=eee&logo=Github&style=for-the-badge" alt="@tanho63" /></a>

## **Citations**

To cite the
[**`fastRhockey`**](https://benhowell71.github.io/fastRhockey/) R
package in publications, use:

BibTex Citation

``` bibtex
@misc{howell_fastRhockey_2021,
  author = {Ben Howell},
  title = {fastRhockey: The SportsDataverse's R Package for Women's Hockey Data.},
  url = {https://benhowell71.github.io/fastRhockey/},
  year = {2021}
}
```
