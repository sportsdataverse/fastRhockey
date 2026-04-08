# **PWHL Teams**

PWHL Teams lookup

## Usage

``` r
pwhl_teams()
```

## Value

A data frame with columns:

- `team_name` - Full team name (e.g., "PWHL Boston")

- `team_id` - Numeric team identifier

- `team_code` - Three-letter team code (e.g., "BOS")

- `team_nickname` - Team nickname

- `team_label` - Short city label (e.g., "Boston")

- `division` - Division identifier

- `team_logo` - URL to team logo image

## Examples

``` r
# \donttest{
  try(pwhl_teams())
#>        team_name team_id team_code  team_nickname team_label division
#> 1    PWHL Boston       1       BOS    PWHL Boston     Boston        1
#> 2 PWHL Minnesota       2       MIN PWHL Minnesota  Minnesota        1
#> 3  PWHL Montreal       3       MON  PWHL Montreal   Montreal        1
#> 4  PWHL New York       4        NY  PWHL New York   New York        1
#> 5    PWHL Ottawa       5       OTT    PWHL Ottawa     Ottawa        1
#> 6   PWHL Toronto       6       TOR   PWHL Toronto    Toronto        1
#>                                              team_logo
#> 1 https://assets.leaguestat.com/pwhl/logos/50x50/1.png
#> 2 https://assets.leaguestat.com/pwhl/logos/50x50/2.png
#> 3 https://assets.leaguestat.com/pwhl/logos/50x50/3.png
#> 4 https://assets.leaguestat.com/pwhl/logos/50x50/4.png
#> 5 https://assets.leaguestat.com/pwhl/logos/50x50/5.png
#> 6 https://assets.leaguestat.com/pwhl/logos/50x50/6.png
# }
```
