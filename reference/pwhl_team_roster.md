# **PWHL Rosters**

PWHL Rosters lookup

## Usage

``` r
pwhl_team_roster(team, season, regular = TRUE)
```

## Arguments

- team:

  Team to pull the roster data for

- season:

  Season (YYYY) to pull the roster from, the concluding year in XXXX-YY
  format

- regular:

  Bool for whether to pull regular or pre-season rosters

## Value

A data frame with roster data

## Examples

``` r
# \donttest{
  try(pwhl_team_roster(season = 2023, team = "Toronto"))
#>    player_id team_id season          player_name first_name      last_name
#> 1        101       6   2023          Alexa Vasko      Alexa          Vasko
#> 2        126       6   2023       Samantha Cogan   Samantha          Cogan
#> 3         65       6   2023        Jesse Compher      Jesse        Compher
#> 4         72       6   2023       Rebecca Leslie    Rebecca         Leslie
#> 5         75       6   2023          Sarah Nurse      Sarah          Nurse
#> 6         66       6   2023       Maggie Connors     Maggie        Connors
#> 7        127       6   2023           Jess Jones       Jess          Jones
#> 8        100       6   2023      Natalie Spooner    Natalie        Spooner
#> 9         73       6   2023         Emma Maltais       Emma        Maltais
#> 10       128       6   2023   Kaitlin Willoughby    Kaitlin     Willoughby
#> 11        89       6   2023        Hannah Miller     Hannah         Miller
#> 12        76       6   2023      Blayre Turnbull     Blayre       Turnbull
#> 13        69       6   2023      Brittany Howard   Brittany         Howard
#> 14       125       6   2023        Victoria Bach   Victoria           Bach
#> 15        71       6   2023    Jocelyne Larocque   Jocelyne       Larocque
#> 16       133       6   2023     Lauriane Rougeau   Lauriane        Rougeau
#> 17        68       6   2023        Kali Flanagan       Kali       Flanagan
#> 18       131       6   2023       Olivia Knowles     Olivia        Knowles
#> 19        74       6   2023         Allie Munroe      Allie         Munroe
#> 20        67       6   2023          Renata Fast     Renata           Fast
#> 21       130       6   2023          Emma Keenan       Emma         Keenan
#> 22       132       6   2023       Jessica Kondas    Jessica         Kondas
#> 23       165       6   2023 Maude Poulin-Labelle      Maude Poulin-Labelle
#> 24        70       6   2023           Erica Howe      Erica           Howe
#> 25        64       6   2023     Kristen Campbell    Kristen       Campbell
#> 26       134       6   2023        Carly Jackson      Carly        Jackson
#>    primary_hand        dob height position            home_town league age
#> 1             L 1999-02-07     NA        F   St. Catharines, ON   pwhl  24
#> 2             L 1997-07-07     NA        F           Ottawa, ON   pwhl  25
#> 3             R 1999-07-01     NA        F       Northbrook, IL   pwhl  24
#> 4             R 1996-05-08     NA        F           Ottawa, ON   pwhl  27
#> 5             L 1995-01-04     NA        F         Hamilton, ON   pwhl  28
#> 6             L 2000-10-22     NA        F       St. John's, NL   pwhl  22
#> 7             L 1990-08-30     NA        F           Picton, ON   pwhl  32
#> 8             R 1990-10-17     NA        F      Scarborough, ON   pwhl  32
#> 9             L 1999-11-04     NA        F       Burlington, ON   pwhl  23
#> 10            R 1995-03-26     NA        F    Prince Albert, SK   pwhl  28
#> 11            L 1996-02-16     NA        F North Vancouver, BC    pwhl  27
#> 12            R 1993-07-15     NA        F       Stellarton, NS   pwhl  29
#> 13            R 1995-11-20     NA        F       St. Thomas, ON   pwhl  27
#> 14            L 1996-07-12     NA        F           Milton, ON   pwhl  26
#> 15            L 1988-05-19     NA        D        Ste. Anne, MB   pwhl  35
#> 16            L 1990-04-12     NA        D     Beaconsfield, QC   pwhl  33
#> 17            R 1995-09-19     NA        D       Burlington, MA   pwhl  27
#> 18            R 1999-01-24     NA        D   Campbell River, BC   pwhl  24
#> 19            L 1997-04-20     NA        D         Yarmouth, NS   pwhl  26
#> 20            R 1994-10-06     NA        D       Burlington, ON   pwhl  28
#> 21            L 1997-11-26     NA        D          Calgary, AB   pwhl  25
#> 22            R 2000-01-03     NA        D          Calgary, AB   pwhl  23
#> 23            L 1999-11-23     NA        D       Sherbrooke, QC   pwhl  23
#> 24            L 1992-07-17     NA        G          Orleans, ON   pwhl  30
#> 25            L 1997-11-30     NA        G          Brandon, MB   pwhl  25
#> 26            L 1997-06-23     NA        G          Amherst, NS   pwhl  26
#>                                       player_headshot regular_season    team
#> 1  https://assets.leaguestat.com/pwhl/240x240/101.jpg           TRUE Toronto
#> 2  https://assets.leaguestat.com/pwhl/240x240/126.jpg           TRUE Toronto
#> 3   https://assets.leaguestat.com/pwhl/240x240/65.jpg           TRUE Toronto
#> 4   https://assets.leaguestat.com/pwhl/240x240/72.jpg           TRUE Toronto
#> 5   https://assets.leaguestat.com/pwhl/240x240/75.jpg           TRUE Toronto
#> 6   https://assets.leaguestat.com/pwhl/240x240/66.jpg           TRUE Toronto
#> 7  https://assets.leaguestat.com/pwhl/240x240/127.jpg           TRUE Toronto
#> 8  https://assets.leaguestat.com/pwhl/240x240/100.jpg           TRUE Toronto
#> 9   https://assets.leaguestat.com/pwhl/240x240/73.jpg           TRUE Toronto
#> 10 https://assets.leaguestat.com/pwhl/240x240/128.jpg           TRUE Toronto
#> 11  https://assets.leaguestat.com/pwhl/240x240/89.jpg           TRUE Toronto
#> 12  https://assets.leaguestat.com/pwhl/240x240/76.jpg           TRUE Toronto
#> 13  https://assets.leaguestat.com/pwhl/240x240/69.jpg           TRUE Toronto
#> 14 https://assets.leaguestat.com/pwhl/240x240/125.jpg           TRUE Toronto
#> 15  https://assets.leaguestat.com/pwhl/240x240/71.jpg           TRUE Toronto
#> 16 https://assets.leaguestat.com/pwhl/240x240/133.jpg           TRUE Toronto
#> 17  https://assets.leaguestat.com/pwhl/240x240/68.jpg           TRUE Toronto
#> 18 https://assets.leaguestat.com/pwhl/240x240/131.jpg           TRUE Toronto
#> 19  https://assets.leaguestat.com/pwhl/240x240/74.jpg           TRUE Toronto
#> 20  https://assets.leaguestat.com/pwhl/240x240/67.jpg           TRUE Toronto
#> 21 https://assets.leaguestat.com/pwhl/240x240/130.jpg           TRUE Toronto
#> 22 https://assets.leaguestat.com/pwhl/240x240/132.jpg           TRUE Toronto
#> 23 https://assets.leaguestat.com/pwhl/240x240/165.jpg           TRUE Toronto
#> 24  https://assets.leaguestat.com/pwhl/240x240/70.jpg           TRUE Toronto
#> 25  https://assets.leaguestat.com/pwhl/240x240/64.jpg           TRUE Toronto
#> 26 https://assets.leaguestat.com/pwhl/240x240/134.jpg           TRUE Toronto
# }
```
