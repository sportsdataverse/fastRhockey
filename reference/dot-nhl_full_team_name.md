# Build the full NHL team name (place + common) from an api-web team struct

The api-web schedule team object carries the city in `placeName$default`
("Tampa Bay") and the mascot in `commonName$default` ("Lightning")
separately; joining them yields the conventional full team name ("Tampa
Bay Lightning") every other source uses. Vector-safe and null-safe:
returns the place name alone when no common name is present, and a
scalar `NA_character_` only when the team carries no place name at all.

## Usage

``` r
.nhl_full_team_name(team)
```

## Arguments

- team:

  The `homeTeam` / `awayTeam` struct from the parsed games frame.

## Value

Character vector of full team names (one per game).
