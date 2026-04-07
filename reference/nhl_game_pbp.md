# **NHL Game Play-by-Play**

Returns detailed play-by-play data for a given NHL game, including event
players, coordinates, on-ice skaters, strength states, shot
distance/angle, and shift integration. Uses the new NHL API at
api-web.nhle.com.

## Usage

``` r
nhl_game_pbp(game_id, include_shifts = TRUE, raw = FALSE)
```

## Arguments

- game_id:

  Game unique ID (e.g. 2024020001)

- include_shifts:

  Logical; whether to integrate shift data for on-ice player tracking.
  Default TRUE.

- raw:

  Logical; if TRUE, return the unprocessed API response as a list
  instead of the parsed data frame. Default FALSE.

## Value

A data frame with one row per event (default), or the raw API response
list when `raw = TRUE`.

## Examples

``` r
# \donttest{
  try(nhl_game_pbp(game_id = 2024020001))
#> ── NHL Game PBP from NHL.com ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 10:51:59 UTC
#> # A tibble: 850 × 93
#>    event_type   event secondary_type event_team_abbr event_team_type description
#>    <chr>        <chr> <chr>          <chr>           <chr>           <glue>     
#>  1 CHANGE       Chan… NA             NA              NA              ON: Nicola…
#>  2 CHANGE       Chan… NA             NA              NA              ON: Jacob …
#>  3 FACEOFF      Face… NA             NJD             away            Nico Hisch…
#>  4 PERIOD_START Peri… NA             NA              NA              Start of P…
#>  5 HIT          Hit   NA             BUF             home            Beck Malen…
#>  6 SHOT         Shot  wrist          NJD             away            Simon Neme…
#>  7 GIVEAWAY     Give… NA             NJD             away            Giveaway b…
#>  8 CHANGE       Chan… NA             NA              NA              ON: Bowen …
#>  9 CHANGE       Chan… NA             NA              NA              ON: Timo M…
#> 10 CHANGE       Chan… NA             NA              NA              ON: Jesper…
#> # ℹ 840 more rows
#> # ℹ 87 more variables: period <int>, period_type <chr>, period_time <chr>,
#> #   period_seconds <dbl>, period_seconds_remaining <dbl>,
#> #   period_time_remaining <chr>, game_seconds <dbl>,
#> #   game_seconds_remaining <dbl>, home_score <int>, away_score <int>,
#> #   event_player_1_name <chr>, event_player_1_type <chr>,
#> #   event_player_1_id <int>, event_player_2_name <chr>, …
  try(nhl_game_pbp(game_id = 2024020001, raw = TRUE))
#> $id
#> [1] 2024020001
#> 
#> $season
#> [1] 20242025
#> 
#> $gameType
#> [1] 2
#> 
#> $limitedScoring
#> [1] FALSE
#> 
#> $gameDate
#> [1] "2024-10-04"
#> 
#> $venue
#> $venue$default
#> [1] "O2 Czech Republic"
#> 
#> 
#> $venueLocation
#> $venueLocation$default
#> [1] "Prague"
#> 
#> $venueLocation$cs
#> [1] "Praha"
#> 
#> $venueLocation$de
#> [1] "Prag"
#> 
#> $venueLocation$fi
#> [1] "Praha"
#> 
#> $venueLocation$sk
#> [1] "Praha"
#> 
#> $venueLocation$sv
#> [1] "Prag"
#> 
#> 
#> $startTimeUTC
#> [1] "2024-10-04T17:00:00Z"
#> 
#> $easternUTCOffset
#> [1] "-04:00"
#> 
#> $venueUTCOffset
#> [1] "+02:00"
#> 
#> $tvBroadcasts
#>    id market countryCode network sequenceNumber
#> 1 324      N          US    NHLN             35
#> 2 282      N          CA      SN            107
#> 3  28      H          US   MSG-B            392
#> 4 409      A          US   MSGSN            411
#> 
#> $gameState
#> [1] "OFF"
#> 
#> $gameScheduleState
#> [1] "OK"
#> 
#> $specialEvent
#> $specialEvent$parentId
#> [1] 36
#> 
#> $specialEvent$name
#> $specialEvent$name$default
#> [1] "2024 NHL Global Series"
#> 
#> 
#> $specialEvent$lightLogoUrl
#> $specialEvent$lightLogoUrl$default
#> [1] "https://assets.nhle.com/special_event_season/20242025/svg/gs-cz-2024-extwm-en_light.svg"
#> 
#> 
#> 
#> $periodDescriptor
#> $periodDescriptor$number
#> [1] 3
#> 
#> $periodDescriptor$periodType
#> [1] "REG"
#> 
#> $periodDescriptor$maxRegulationPeriods
#> [1] 3
#> 
#> 
#> $awayTeam
#> $awayTeam$id
#> [1] 1
#> 
#> $awayTeam$commonName
#> $awayTeam$commonName$default
#> [1] "Devils"
#> 
#> 
#> $awayTeam$abbrev
#> [1] "NJD"
#> 
#> $awayTeam$score
#> [1] 4
#> 
#> $awayTeam$sog
#> [1] 23
#> 
#> $awayTeam$logo
#> [1] "https://assets.nhle.com/logos/nhl/svg/NJD_light.svg"
#> 
#> $awayTeam$darkLogo
#> [1] "https://assets.nhle.com/logos/nhl/svg/NJD_dark.svg"
#> 
#> $awayTeam$placeName
#> $awayTeam$placeName$default
#> [1] "New Jersey"
#> 
#> 
#> $awayTeam$placeNameWithPreposition
#> $awayTeam$placeNameWithPreposition$default
#> [1] "New Jersey"
#> 
#> $awayTeam$placeNameWithPreposition$fr
#> [1] "du New Jersey"
#> 
#> 
#> 
#> $homeTeam
#> $homeTeam$id
#> [1] 7
#> 
#> $homeTeam$commonName
#> $homeTeam$commonName$default
#> [1] "Sabres"
#> 
#> 
#> $homeTeam$abbrev
#> [1] "BUF"
#> 
#> $homeTeam$score
#> [1] 1
#> 
#> $homeTeam$sog
#> [1] 31
#> 
#> $homeTeam$logo
#> [1] "https://assets.nhle.com/logos/nhl/svg/BUF_light.svg"
#> 
#> $homeTeam$darkLogo
#> [1] "https://assets.nhle.com/logos/nhl/svg/BUF_dark.svg"
#> 
#> $homeTeam$placeName
#> $homeTeam$placeName$default
#> [1] "Buffalo"
#> 
#> 
#> $homeTeam$placeNameWithPreposition
#> $homeTeam$placeNameWithPreposition$default
#> [1] "Buffalo"
#> 
#> $homeTeam$placeNameWithPreposition$fr
#> [1] "de Buffalo"
#> 
#> 
#> 
#> $shootoutInUse
#> [1] TRUE
#> 
#> $otInUse
#> [1] TRUE
#> 
#> $clock
#> $clock$timeRemaining
#> [1] "00:00"
#> 
#> $clock$secondsRemaining
#> [1] 0
#> 
#> $clock$running
#> [1] FALSE
#> 
#> $clock$inIntermission
#> [1] FALSE
#> 
#> 
#> $displayPeriod
#> [1] 1
#> 
#> $maxPeriods
#> [1] 5
#> 
#> $gameOutcome
#> $gameOutcome$lastPeriodType
#> [1] "REG"
#> 
#> 
#> $plays
#>     eventId timeInPeriod timeRemaining situationCode homeTeamDefendingSide
#> 1       152        00:00         20:00          1551                 right
#> 2       151        00:00         20:00          1551                 right
#> 3       147        00:05         19:55          1551                 right
#> 4       103        00:08         19:52          1551                 right
#> 5       123        00:19         19:41          1551                 right
#> 6       302        00:29         19:31          1551                 right
#> 7       208        00:36         19:24          1551                 right
#> 8       112        00:40         19:20          1551                 right
#> 9       113        00:46         19:14          1551                 right
#> 10       57        00:56         19:04          1551                 right
#> 11      153        00:56         19:04          1551                 right
#> 12      119        00:57         19:03          1551                 right
#> 13      120        01:02         18:58          1551                 right
#> 14      230        01:03         18:57          1551                 right
#> 15       58        01:04         18:56          1551                 right
#> 16      154        01:04         18:56          1551                 right
#> 17      124        01:06         18:54          1551                 right
#> 18      255        01:34         18:26          1560                 right
#> 19      135        01:37         18:23          1560                 right
#> 20      155        01:37         18:23          1451                 right
#> 21      260        01:58         18:02          1451                 right
#> 22  1731721        02:14         17:46          1451                 right
#> 23      278        02:18         17:42          1451                 right
#> 24      243        03:02         16:58          1451                 right
#> 25      156        03:19         16:41          1451                 right
#> 26      301        03:20         16:40          1451                 right
#> 27      204        03:38         16:22          1551                 right
#> 28      206        03:46         16:14          1551                 right
#> 29      244        03:54         16:06          1551                 right
#> 30      214        04:03         15:57          1551                 right
#> 31      296        04:10         15:50          1551                 right
#> 32      298        04:18         15:42          1551                 right
#> 33      223        04:47         15:13          1551                 right
#> 34      246        05:00         15:00          1551                 right
#> 35       60        05:09         14:51          1551                 right
#> 36      157        05:09         14:51          1551                 right
#> 37  1730555        06:02         13:58          1551                 right
#> 38      239        06:07         13:53          1551                 right
#> 39      240        06:12         13:48          1551                 right
#> 40       61        06:14         13:46          1551                 right
#> 41      158        06:14         13:46          1551                 right
#> 42  1730569        06:18         13:42          1551                 right
#> 43  1730582        06:19         13:41          1551                 right
#> 44  1730598        06:30         13:30          1551                 right
#> 45  1730606        06:31         13:29          1551                 right
#> 46      259        06:32         13:28          1551                 right
#> 47      272        07:08         12:52          1551                 right
#> 48      254        07:18         12:42          1551                 right
#> 49       62        07:21         12:39          1551                 right
#> 50      159        07:21         12:39          1551                 right
#> 51  1730613        07:35         12:25          1551                 right
#> 52  1731722        07:46         12:14          1551                 right
#> 53  1730619        07:50         12:10          1551                 right
#> 54      271        08:26         11:34          1551                 right
#> 55  1730623        08:31         11:29          1551                 right
#> 56  1730627        08:33         11:27          1551                 right
#> 57      274        08:39         11:21          1551                 right
#> 58      160        08:39         11:21          1551                 right
#> 59      279        09:00         11:00          1551                 right
#> 60      280        09:04         10:56          1551                 right
#> 61      281        09:06         10:54          1551                 right
#> 62  1730706        09:09         10:51          1551                 right
#> 63      282        09:09         10:51          1551                 right
#> 64      161        09:12         10:48          1551                 right
#> 65       63        09:14         10:46          1551                 right
#> 66      162        09:14         10:46          1551                 right
#> 67      286        09:41         10:19          1551                 right
#> 68  1730551        09:44         10:16          1551                 right
#> 69       64        10:15         09:45          1551                 right
#> 70      163        10:15         09:45          1551                 right
#> 71  1730553        10:34         09:26          1551                 right
#> 72  1730558        10:38         09:22          1551                 right
#> 73      164        10:38         09:22          1541                 right
#> 74  1730561        10:42         09:18          1541                 right
#> 75  1730562        10:48         09:12          1541                 right
#> 76  1730571        11:05         08:55          1541                 right
#> 77  1730729        11:06         08:54          1541                 right
#> 78  1730566        11:31         08:29          1541                 right
#> 79  1730567        11:32         08:28          1541                 right
#> 80  1730568        11:43         08:17          1541                 right
#> 81  1730570        12:01         07:59          1541                 right
#> 82  1730737        13:04         06:56          1551                 right
#> 83  1730612        13:46         06:14          1551                 right
#> 84  1730602        14:05         05:55          1551                 right
#> 85      401        14:06         05:54          1551                 right
#> 86  1730607        14:27         05:33          1551                 right
#> 87       65        14:29         05:31          1551                 right
#> 88      165        14:29         05:31          1551                 right
#> 89  1730738        14:47         05:13          1551                 right
#> 90  1730651        15:09         04:51          1551                 right
#> 91       66        15:10         04:50          1551                 right
#> 92      166        15:10         04:50          1551                 right
#> 93  1730620        15:38         04:22          1551                 right
#> 94      167        15:38         04:22          1551                 right
#> 95       67        15:45         04:15          1551                 right
#> 96      168        15:45         04:15          1551                 right
#> 97  1730626        15:54         04:06          1551                 right
#> 98       68        15:56         04:04          1551                 right
#> 99      169        15:56         04:04          1551                 right
#> 100 1730739        16:07         03:53          1551                 right
#> 101 1730740        16:15         03:45          1551                 right
#> 102 1730639        16:46         03:14          1551                 right
#> 103 1731723        16:51         03:09          1551                 right
#> 104     353        16:57         03:03          1551                 right
#> 105 1730641        16:57         03:03          1551                 right
#> 106 1730741        17:10         02:50          1551                 right
#> 107      69        17:11         02:49          1551                 right
#> 108 1730703        17:37         02:23          0651                 right
#> 109     170        17:37         02:23          1541                 right
#> 110      70        18:05         01:55          1541                 right
#> 111     171        18:05         01:55          1541                 right
#> 112      71        18:22         01:38          1541                 right
#> 113     172        18:22         01:38          1541                 right
#> 114 1730713        18:37         01:23          1541                 right
#> 115 1730734        19:16         00:44          1541                 right
#> 116 1730720        19:18         00:42          1541                 right
#> 117 1730728        19:31         00:29          1541                 right
#> 118 1730735        19:33         00:27          1541                 right
#> 119 1730742        19:35         00:25          1541                 right
#> 120 1730736        19:35         00:25          1541                 right
#> 121 1730731        19:44         00:16          1551                 right
#> 122      72        20:00         00:00          1551                 right
#> 123     174        00:00         20:00          1551                  left
#> 124     173        00:00         20:00          1551                  left
#> 125      77        00:14         19:46          1551                  left
#> 126     175        00:14         19:46          1551                  left
#> 127 1730746        00:32         19:28          1551                  left
#> 128 1730747        00:35         19:25          1551                  left
#> 129 1730870        00:55         19:05          1551                  left
#> 130 1730855        00:57         19:03          1551                  left
#> 131 1730858        01:12         18:48          1551                  left
#> 132 1730887        01:49         18:11          1551                  left
#> 133      78        01:51         18:09          1551                  left
#> 134     176        01:51         18:09          1551                  left
#> 135 1730869        01:56         18:04          1551                  left
#> 136 1730880        02:10         17:50          1551                  left
#> 137 1730871        02:13         17:47          1551                  left
#> 138 1730652        02:15         17:45          1551                  left
#> 139 1730885        02:19         17:41          1551                  left
#> 140 1730907        02:23         17:37          1551                  left
#> 141 1730908        02:27         17:33          1551                  left
#> 142 1731724        03:17         16:43          1551                  left
#> 143 1730881        03:25         16:35          1551                  left
#> 144     304        03:27         16:33          1551                  left
#> 145 1730882        03:29         16:31          1551                  left
#> 146     177        03:29         16:31          1551                  left
#> 147 1730891        04:13         15:47          1551                  left
#> 148 1730895        04:19         15:41          1551                  left
#> 149 1730932        04:28         15:32          1551                  left
#> 150 1730935        04:47         15:13          1551                  left
#> 151 1730653        05:08         14:52          1551                  left
#> 152      79        05:09         14:51          1551                  left
#> 153     178        05:09         14:51          1551                  left
#> 154 1730918        05:30         14:30          1551                  left
#> 155 1730951        05:46         14:14          1551                  left
#> 156 1730922        05:49         14:11          1551                  left
#> 157 1730923        05:59         14:01          1551                  left
#> 158 1730914        06:08         13:52          1551                  left
#> 159 1730919        06:15         13:45          1551                  left
#> 160      80        06:15         13:45          1451                  left
#> 161     179        06:15         13:45          1451                  left
#> 162      81        06:51         13:09          1451                  left
#> 163     180        06:51         13:09          1451                  left
#> 164 1730929        07:12         12:48          1451                  left
#> 165      82        07:15         12:45          1451                  left
#> 166     181        07:15         12:45          1451                  left
#> 167 1730933        07:42         12:18          1451                  left
#> 168 1730934        07:49         12:11          1451                  left
#> 169 1730936        08:01         11:59          1451                  left
#> 170 1730937        08:11         11:49          1451                  left
#> 171 1730940        08:18         11:42          1551                  left
#> 172      83        08:20         11:40          1551                  left
#> 173     182        08:20         11:40          1551                  left
#> 174 1730944        08:22         11:38          1551                  left
#> 175 1730973        08:33         11:27          1551                  left
#> 176 1731725        08:34         11:26          1551                  left
#> 177 1730947        08:51         11:09          1551                  left
#> 178 1730962        08:59         11:01          1551                  left
#> 179 1730964        09:27         10:33          1551                  left
#> 180 1730952        09:28         10:32          1551                  left
#> 181 1730953        09:38         10:22          1551                  left
#> 182     354        09:41         10:19          1551                  left
#> 183 1730996        09:53         10:07          1551                  left
#> 184 1731001        09:56         10:04          1551                  left
#> 185      84        10:06         09:54          1551                  left
#> 186     183        10:06         09:54          1551                  left
#> 187 1730965        10:15         09:45          1551                  left
#> 188 1731017        10:18         09:42          1551                  left
#> 189 1731018        10:28         09:32          1551                  left
#> 190 1731019        10:36         09:24          1551                  left
#> 191 1730972        11:32         08:28          1551                  left
#> 192 1730655        12:20         07:40          1551                  left
#> 193     184        12:29         07:31          1551                  left
#> 194 1731726        12:39         07:21          1551                  left
#> 195      85        12:57         07:03          1551                  left
#> 196     185        12:57         07:03          1551                  left
#> 197 1731002        13:03         06:57          1551                  left
#> 198 1730992        13:06         06:54          1551                  left
#> 199 1731004        13:13         06:47          1551                  left
#> 200 1730993        13:14         06:46          1551                  left
#> 201 1730994        13:20         06:40          1551                  left
#> 202 1731039        13:23         06:37          1551                  left
#> 203 1730995        13:29         06:31          1551                  left
#> 204 1730997        13:31         06:29          1551                  left
#> 205      86        13:40         06:20          1551                  left
#> 206     186        13:40         06:20          1551                  left
#> 207 1731015        13:46         06:14          1551                  left
#> 208 1731005        14:12         05:48          1551                  left
#> 209 1731006        14:15         05:45          1551                  left
#> 210 1731010        14:26         05:34          1551                  left
#> 211      87        14:28         05:32          1551                  left
#> 212     187        14:28         05:32          1551                  left
#> 213      88        14:44         05:16          1551                  left
#> 214 1731026        15:07         04:53          1560                  left
#> 215     188        15:07         04:53          1451                  left
#> 216 1731041        15:29         04:31          1451                  left
#> 217 1731031        15:41         04:19          1451                  left
#> 218 1731035        16:04         03:56          1451                  left
#> 219      89        16:07         03:53          1451                  left
#> 220     189        16:07         03:53          1451                  left
#> 221 1731055        16:31         03:29          1451                  left
#> 222 1731043        16:47         03:13          1451                  left
#> 223 1731046        17:07         02:53          1551                  left
#> 224 1731059        17:59         02:01          1551                  left
#> 225      90        18:19         01:41          1551                  left
#> 226     190        18:19         01:41          1551                  left
#> 227 1731068        18:21         01:39          1551                  left
#> 228 1731089        18:47         01:13          1551                  left
#> 229 1731071        18:56         01:04          1551                  left
#> 230 1731090        19:24         00:36          1551                  left
#> 231 1731088        19:32         00:28          1551                  left
#> 232      91        19:40         00:20          1551                  left
#> 233     191        19:40         00:20          1551                  left
#> 234      92        19:49         00:11          1551                  left
#> 235     192        19:49         00:11          1551                  left
#> 236      93        20:00         00:00          1551                  left
#> 237     194        00:00         20:00          1551                 right
#> 238     193        00:00         20:00          1551                 right
#> 239 1731103        00:05         19:55          1551                 right
#> 240      98        00:21         19:39          1551                 right
#> 241     195        00:21         19:39          1551                 right
#> 242 1731115        00:35         19:25          1551                 right
#> 243 1731097        00:41         19:19          1551                 right
#> 244      99        00:44         19:16          1551                 right
#> 245     196        00:44         19:16          1551                 right
#> 246 1731727        01:08         18:52          1551                 right
#> 247 1731134        01:22         18:38          1551                 right
#> 248 1731104        01:32         18:28          1551                 right
#> 249     100        01:59         18:01          1551                 right
#> 250     197        01:59         18:01          1551                 right
#> 251 1731728        02:04         17:56          1551                 right
#> 252 1731163        02:57         17:03          1551                 right
#> 253 1731152        02:58         17:02          1551                 right
#> 254 1731122        03:01         16:59          1551                 right
#> 255 1731170        03:05         16:55          1551                 right
#> 256 1731133        03:41         16:19          1551                 right
#> 257 1731169        04:30         15:30          1551                 right
#> 258 1731144        04:42         15:18          1551                 right
#> 259 1731151        05:03         14:57          1551                 right
#> 260 1731177        05:26         14:34          1551                 right
#> 261 1731180        05:35         14:25          1551                 right
#> 262 1731164        05:43         14:17          1551                 right
#> 263     198        05:43         14:17          1451                 right
#> 264 1731175        06:40         13:20          1451                 right
#> 265 1731176        06:41         13:19          1451                 right
#> 266 1730801        06:43         13:17          1451                 right
#> 267     199        06:43         13:17          1451                 right
#> 268     355        06:49         13:11          1451                 right
#> 269 1731198        06:52         13:08          1451                 right
#> 270 1731183        07:02         12:58          1451                 right
#> 271 1730802        07:05         12:55          1451                 right
#> 272     200        07:05         12:55          1451                 right
#> 273 1731195        07:59         12:01          1551                 right
#> 274 1730803        08:01         11:59          1551                 right
#> 275 1730751        08:01         11:59          1551                 right
#> 276 1731199        08:10         11:50          1551                 right
#> 277 1731206        08:25         11:35          1551                 right
#> 278 1731200        08:25         11:35          1551                 right
#> 279 1730804        08:28         11:32          1551                 right
#> 280 1730752        08:28         11:32          1551                 right
#> 281 1731207        08:37         11:23          1551                 right
#> 282 1730805        08:39         11:21          1551                 right
#> 283 1730753        08:39         11:21          1551                 right
#> 284 1731229        09:04         10:56          1551                 right
#> 285 1731232        09:09         10:51          1551                 right
#> 286 1731233        09:26         10:34          1551                 right
#> 287 1731217        09:33         10:27          1551                 right
#> 288 1731228        09:37         10:23          1551                 right
#> 289 1731234        09:38         10:22          1551                 right
#> 290 1731247        09:40         10:20          1551                 right
#> 291 1731218        09:46         10:14          1551                 right
#> 292 1731729        09:52         10:08          1551                 right
#> 293 1731730        09:54         10:06          1551                 right
#> 294 1731222        10:02         09:58          1551                 right
#> 295 1731223        10:03         09:57          1551                 right
#> 296 1731264        10:06         09:54          1551                 right
#> 297 1731224        10:07         09:53          1551                 right
#> 298 1730754        10:07         09:53          1551                 right
#> 299 1731267        10:11         09:49          1551                 right
#> 300 1730806        10:16         09:44          1551                 right
#> 301 1730755        10:16         09:44          1551                 right
#> 302 1731731        10:52         09:08          1551                 right
#> 303 1731246        11:21         08:39          1551                 right
#> 304 1730807        11:29         08:31          1551                 right
#> 305 1730756        11:29         08:31          1551                 right
#> 306 1731254        11:44         08:16          1551                 right
#> 307 1731268        11:59         08:01          1551                 right
#> 308 1731270        12:20         07:40          1551                 right
#> 309 1731732        12:23         07:37          1551                 right
#> 310 1730808        12:31         07:29          1551                 right
#> 311 1730757        12:31         07:29          1551                 right
#> 312 1730809        12:38         07:22          1551                 right
#> 313 1730758        12:38         07:22          1551                 right
#> 314 1731271        12:57         07:03          1551                 right
#> 315 1731281        13:37         06:23          1551                 right
#> 316 1731733        13:41         06:19          1551                 right
#> 317 1731282        13:43         06:17          1551                 right
#> 318 1730810        13:46         06:14          1551                 right
#> 319 1730759        13:46         06:14          1551                 right
#> 320 1730811        13:55         06:05          1551                 right
#> 321 1730760        13:55         06:05          1551                 right
#> 322 1731293        14:32         05:28          1551                 right
#> 323 1730812        14:34         05:26          1551                 right
#> 324 1730761        14:34         05:26          1551                 right
#> 325 1731297        14:41         05:19          1551                 right
#> 326 1731298        14:50         05:10          1551                 right
#> 327     356        14:51         05:09          1551                 right
#> 328 1731351        14:52         05:08          1551                 right
#> 329 1731299        15:07         04:53          1551                 right
#> 330 1730813        15:15         04:45          1551                 right
#> 331 1730762        15:15         04:45          1551                 right
#> 332 1731305        15:29         04:31          1551                 right
#> 333 1731352        17:20         02:40          1560                 right
#> 334 1731541        17:20         02:40          1560                 right
#> 335 1731328        17:28         02:32          1560                 right
#> 336 1730763        17:28         02:32          1551                 right
#> 337 1731409        18:43         01:17          1551                 right
#> 338 1731343        18:46         01:14          1551                 right
#> 339 1730814        18:49         01:11          1551                 right
#> 340 1730764        18:49         01:11          1551                 right
#> 341 1731411        19:07         00:53          1551                 right
#> 342 1731348        19:15         00:45          1551                 right
#> 343 1731350        19:25         00:35          1551                 right
#> 344 1730815        19:46         00:14          1551                 right
#> 345 1730765        19:46         00:14          1551                 right
#> 346 1731412        19:49         00:11          1551                 right
#> 347 1731413        19:51         00:09          1551                 right
#> 348 1730816        20:00         00:00          1551                 right
#> 349 1730820        20:00         00:00          1551                 right
#>     typeCode     typeDescKey sortOrder
#> 1        520    period-start        10
#> 2        502         faceoff        11
#> 3        503             hit        12
#> 4        506    shot-on-goal        13
#> 5        504        giveaway        14
#> 6        507     missed-shot        21
#> 7        503             hit        23
#> 8        506    shot-on-goal        24
#> 9        508    blocked-shot        25
#> 10       516        stoppage        28
#> 11       502         faceoff        29
#> 12       508    blocked-shot        30
#> 13       506    shot-on-goal        31
#> 14       503             hit        32
#> 15       516        stoppage        33
#> 16       502         faceoff        35
#> 17       507     missed-shot        36
#> 18       503             hit        44
#> 19       509         penalty        45
#> 20       502         faceoff        49
#> 21       503             hit        50
#> 22       503             hit        54
#> 23       503             hit        55
#> 24       504        giveaway        63
#> 25       507     missed-shot        67
#> 26       506    shot-on-goal        68
#> 27       506    shot-on-goal        72
#> 28       506    shot-on-goal        73
#> 29       504        giveaway        76
#> 30       507     missed-shot        81
#> 31       503             hit        82
#> 32       503             hit        83
#> 33       507     missed-shot        92
#> 34       504        giveaway        95
#> 35       516        stoppage        96
#> 36       502         faceoff        99
#> 37       503             hit       106
#> 38       507     missed-shot       107
#> 39       508    blocked-shot       108
#> 40       516        stoppage       109
#> 41       502         faceoff       112
#> 42       503             hit       113
#> 43       503             hit       114
#> 44       503             hit       115
#> 45       503             hit       116
#> 46       504        giveaway       117
#> 47       504        giveaway       124
#> 48       506    shot-on-goal       125
#> 49       516        stoppage       126
#> 50       502         faceoff       129
#> 51       503             hit       130
#> 52       503             hit       131
#> 53       503             hit       132
#> 54       506    shot-on-goal       143
#> 55       503             hit       144
#> 56       503             hit       145
#> 57       505            goal       146
#> 58       502         faceoff       149
#> 59       508    blocked-shot       150
#> 60       508    blocked-shot       151
#> 61       506    shot-on-goal       152
#> 62       503             hit       153
#> 63       507     missed-shot       154
#> 64       506    shot-on-goal       155
#> 65       516        stoppage       156
#> 66       502         faceoff       159
#> 67       508    blocked-shot       160
#> 68       504        giveaway       161
#> 69       516        stoppage       170
#> 70       502         faceoff       173
#> 71       508    blocked-shot       174
#> 72       509         penalty       175
#> 73       502         faceoff       179
#> 74       508    blocked-shot       180
#> 75       508    blocked-shot       181
#> 76       504        giveaway       182
#> 77       503             hit       183
#> 78       506    shot-on-goal       186
#> 79       508    blocked-shot       187
#> 80       508    blocked-shot       188
#> 81       508    blocked-shot       189
#> 82       503             hit       207
#> 83       504        giveaway       217
#> 84       508    blocked-shot       219
#> 85       508    blocked-shot       220
#> 86       506    shot-on-goal       225
#> 87       516        stoppage       226
#> 88       502         faceoff       229
#> 89       503             hit       230
#> 90       508    blocked-shot       233
#> 91       516        stoppage       234
#> 92       502         faceoff       237
#> 93       505            goal       238
#> 94       502         faceoff       241
#> 95       516        stoppage       242
#> 96       502         faceoff       243
#> 97       506    shot-on-goal       244
#> 98       516        stoppage       245
#> 99       502         faceoff       247
#> 100      503             hit       248
#> 101      503             hit       250
#> 102      506    shot-on-goal       259
#> 103      503             hit       260
#> 104      508    blocked-shot       261
#> 105      506    shot-on-goal       262
#> 106      503             hit       263
#> 107      535 delayed-penalty       264
#> 108      509         penalty       273
#> 109      502         faceoff       277
#> 110      516        stoppage       278
#> 111      502         faceoff       281
#> 112      516        stoppage       284
#> 113      502         faceoff       285
#> 114      506    shot-on-goal       286
#> 115      504        giveaway       293
#> 116      508    blocked-shot       294
#> 117      507     missed-shot       302
#> 118      504        giveaway       303
#> 119      503             hit       304
#> 120      504        giveaway       305
#> 121      507     missed-shot       307
#> 122      521      period-end       309
#> 123      520    period-start       316
#> 124      502         faceoff       317
#> 125      516        stoppage       318
#> 126      502         faceoff       319
#> 127      506    shot-on-goal       320
#> 128      507     missed-shot       321
#> 129      503             hit       328
#> 130      508    blocked-shot       329
#> 131      507     missed-shot       331
#> 132      503             hit       338
#> 133      516        stoppage       339
#> 134      502         faceoff       342
#> 135      506    shot-on-goal       343
#> 136      504        giveaway       344
#> 137      506    shot-on-goal       345
#> 138      507     missed-shot       346
#> 139      504        giveaway       347
#> 140      503             hit       348
#> 141      503             hit       349
#> 142      503             hit       358
#> 143      506    shot-on-goal       360
#> 144      506    shot-on-goal       361
#> 145      505            goal       362
#> 146      502         faceoff       365
#> 147      507     missed-shot       369
#> 148      508    blocked-shot       371
#> 149      503             hit       373
#> 150      503             hit       377
#> 151      506    shot-on-goal       382
#> 152      516        stoppage       383
#> 153      502         faceoff       386
#> 154      504        giveaway       387
#> 155      503             hit       389
#> 156      504        giveaway       390
#> 157      504        giveaway       391
#> 158      506    shot-on-goal       396
#> 159      509         penalty       397
#> 160      516        stoppage       399
#> 161      502         faceoff       402
#> 162      516        stoppage       405
#> 163      502         faceoff       408
#> 164      506    shot-on-goal       409
#> 165      516        stoppage       410
#> 166      502         faceoff       412
#> 167      507     missed-shot       413
#> 168      506    shot-on-goal       414
#> 169      507     missed-shot       415
#> 170      506    shot-on-goal       416
#> 171      506    shot-on-goal       418
#> 172      516        stoppage       419
#> 173      502         faceoff       422
#> 174      506    shot-on-goal       423
#> 175      503             hit       424
#> 176      503             hit       425
#> 177      507     missed-shot       427
#> 178      504        giveaway       429
#> 179      504        giveaway       432
#> 180      507     missed-shot       433
#> 181      506    shot-on-goal       434
#> 182      506    shot-on-goal       435
#> 183      503             hit       440
#> 184      503             hit       442
#> 185      516        stoppage       445
#> 186      502         faceoff       447
#> 187      506    shot-on-goal       448
#> 188      503             hit       449
#> 189      503             hit       450
#> 190      503             hit       451
#> 191      506    shot-on-goal       458
#> 192      508    blocked-shot       469
#> 193      508    blocked-shot       470
#> 194      503             hit       472
#> 195      516        stoppage       477
#> 196      502         faceoff       479
#> 197      504        giveaway       480
#> 198      507     missed-shot       481
#> 199      504        giveaway       482
#> 200      508    blocked-shot       483
#> 201      507     missed-shot       484
#> 202      503             hit       485
#> 203      508    blocked-shot       486
#> 204      508    blocked-shot       487
#> 205      516        stoppage       488
#> 206      502         faceoff       491
#> 207      525        takeaway       492
#> 208      507     missed-shot       493
#> 209      507     missed-shot       494
#> 210      506    shot-on-goal       500
#> 211      516        stoppage       501
#> 212      502         faceoff       504
#> 213      535 delayed-penalty       505
#> 214      509         penalty       508
#> 215      502         faceoff       512
#> 216      525        takeaway       513
#> 217      507     missed-shot       516
#> 218      506    shot-on-goal       521
#> 219      516        stoppage       522
#> 220      502         faceoff       525
#> 221      525        takeaway       526
#> 222      507     missed-shot       527
#> 223      508    blocked-shot       530
#> 224      508    blocked-shot       541
#> 225      516        stoppage       548
#> 226      502         faceoff       550
#> 227      507     missed-shot       551
#> 228      503             hit       554
#> 229      507     missed-shot       555
#> 230      503             hit       565
#> 231      504        giveaway       568
#> 232      516        stoppage       569
#> 233      502         faceoff       572
#> 234      516        stoppage       573
#> 235      502         faceoff       574
#> 236      521      period-end       575
#> 237      520    period-start       582
#> 238      502         faceoff       583
#> 239      504        giveaway       584
#> 240      516        stoppage       585
#> 241      502         faceoff       586
#> 242      503             hit       587
#> 243      506    shot-on-goal       592
#> 244      516        stoppage       594
#> 245      502         faceoff       597
#> 246      503             hit       598
#> 247      503             hit       599
#> 248      507     missed-shot       600
#> 249      516        stoppage       608
#> 250      502         faceoff       609
#> 251      503             hit       610
#> 252      503             hit       619
#> 253      504        giveaway       620
#> 254      506    shot-on-goal       622
#> 255      503             hit       623
#> 256      508    blocked-shot       633
#> 257      525        takeaway       642
#> 258      508    blocked-shot       643
#> 259      506    shot-on-goal       650
#> 260      504        giveaway       654
#> 261      504        giveaway       656
#> 262      509         penalty       660
#> 263      502         faceoff       664
#> 264      506    shot-on-goal       668
#> 265      507     missed-shot       669
#> 266      516        stoppage       670
#> 267      502         faceoff       672
#> 268      508    blocked-shot       673
#> 269      503             hit       674
#> 270      508    blocked-shot       675
#> 271      516        stoppage       676
#> 272      502         faceoff       678
#> 273      506    shot-on-goal       687
#> 274      516        stoppage       688
#> 275      502         faceoff       690
#> 276      507     missed-shot       691
#> 277      504        giveaway       692
#> 278      507     missed-shot       693
#> 279      516        stoppage       694
#> 280      502         faceoff       697
#> 281      506    shot-on-goal       698
#> 282      516        stoppage       699
#> 283      502         faceoff       701
#> 284      504        giveaway       703
#> 285      504        giveaway       705
#> 286      504        giveaway       713
#> 287      507     missed-shot       714
#> 288      503             hit       715
#> 289      503             hit       716
#> 290      504        giveaway       717
#> 291      507     missed-shot       718
#> 292      503             hit       719
#> 293      503             hit       720
#> 294      506    shot-on-goal       724
#> 295      507     missed-shot       725
#> 296      503             hit       726
#> 297      505            goal       727
#> 298      502         faceoff       730
#> 299      503             hit       731
#> 300      516        stoppage       732
#> 301      502         faceoff       734
#> 302      503             hit       740
#> 303      506    shot-on-goal       747
#> 304      516        stoppage       748
#> 305      502         faceoff       750
#> 306      506    shot-on-goal       755
#> 307      525        takeaway       756
#> 308      504        giveaway       763
#> 309      503             hit       765
#> 310      516        stoppage       766
#> 311      502         faceoff       769
#> 312      516        stoppage       770
#> 313      502         faceoff       771
#> 314      508    blocked-shot       772
#> 315      508    blocked-shot       782
#> 316      503             hit       783
#> 317      506    shot-on-goal       784
#> 318      516        stoppage       785
#> 319      502         faceoff       788
#> 320      516        stoppage       789
#> 321      502         faceoff       791
#> 322      506    shot-on-goal       796
#> 323      516        stoppage       797
#> 324      502         faceoff       800
#> 325      506    shot-on-goal       801
#> 326      506    shot-on-goal       802
#> 327      507     missed-shot       803
#> 328      507     missed-shot       804
#> 329      508    blocked-shot       805
#> 330      516        stoppage       806
#> 331      502         faceoff       808
#> 332      506    shot-on-goal       812
#> 333      508    blocked-shot       832
#> 334      508    blocked-shot       833
#> 335      505            goal       834
#> 336      502         faceoff       837
#> 337      504        giveaway       849
#> 338      506    shot-on-goal       850
#> 339      516        stoppage       851
#> 340      502         faceoff       854
#> 341      525        takeaway       855
#> 342      507     missed-shot       857
#> 343      507     missed-shot       858
#> 344      516        stoppage       866
#> 345      502         faceoff       867
#> 346      525        takeaway       868
#> 347      504        giveaway       869
#> 348      521      period-end       870
#> 349      524        game-end       874
#>                                                        pptReplayUrl
#> 1                                                              <NA>
#> 2                                                              <NA>
#> 3                                                              <NA>
#> 4                                                              <NA>
#> 5                                                              <NA>
#> 6                                                              <NA>
#> 7                                                              <NA>
#> 8                                                              <NA>
#> 9                                                              <NA>
#> 10                                                             <NA>
#> 11                                                             <NA>
#> 12                                                             <NA>
#> 13                                                             <NA>
#> 14                                                             <NA>
#> 15                                                             <NA>
#> 16                                                             <NA>
#> 17                                                             <NA>
#> 18                                                             <NA>
#> 19                                                             <NA>
#> 20                                                             <NA>
#> 21                                                             <NA>
#> 22                                                             <NA>
#> 23                                                             <NA>
#> 24                                                             <NA>
#> 25                                                             <NA>
#> 26                                                             <NA>
#> 27                                                             <NA>
#> 28                                                             <NA>
#> 29                                                             <NA>
#> 30                                                             <NA>
#> 31                                                             <NA>
#> 32                                                             <NA>
#> 33                                                             <NA>
#> 34                                                             <NA>
#> 35                                                             <NA>
#> 36                                                             <NA>
#> 37                                                             <NA>
#> 38                                                             <NA>
#> 39                                                             <NA>
#> 40                                                             <NA>
#> 41                                                             <NA>
#> 42                                                             <NA>
#> 43                                                             <NA>
#> 44                                                             <NA>
#> 45                                                             <NA>
#> 46                                                             <NA>
#> 47                                                             <NA>
#> 48                                                             <NA>
#> 49                                                             <NA>
#> 50                                                             <NA>
#> 51                                                             <NA>
#> 52                                                             <NA>
#> 53                                                             <NA>
#> 54                                                             <NA>
#> 55                                                             <NA>
#> 56                                                             <NA>
#> 57      https://wsr.nhle.com/sprites/20242025/2024020001/ev274.json
#> 58                                                             <NA>
#> 59                                                             <NA>
#> 60                                                             <NA>
#> 61                                                             <NA>
#> 62                                                             <NA>
#> 63                                                             <NA>
#> 64                                                             <NA>
#> 65                                                             <NA>
#> 66                                                             <NA>
#> 67                                                             <NA>
#> 68                                                             <NA>
#> 69                                                             <NA>
#> 70                                                             <NA>
#> 71                                                             <NA>
#> 72                                                             <NA>
#> 73                                                             <NA>
#> 74                                                             <NA>
#> 75                                                             <NA>
#> 76                                                             <NA>
#> 77                                                             <NA>
#> 78                                                             <NA>
#> 79                                                             <NA>
#> 80                                                             <NA>
#> 81                                                             <NA>
#> 82                                                             <NA>
#> 83                                                             <NA>
#> 84                                                             <NA>
#> 85                                                             <NA>
#> 86                                                             <NA>
#> 87                                                             <NA>
#> 88                                                             <NA>
#> 89                                                             <NA>
#> 90                                                             <NA>
#> 91                                                             <NA>
#> 92                                                             <NA>
#> 93  https://wsr.nhle.com/sprites/20242025/2024020001/ev1730620.json
#> 94                                                             <NA>
#> 95                                                             <NA>
#> 96                                                             <NA>
#> 97                                                             <NA>
#> 98                                                             <NA>
#> 99                                                             <NA>
#> 100                                                            <NA>
#> 101                                                            <NA>
#> 102                                                            <NA>
#> 103                                                            <NA>
#> 104                                                            <NA>
#> 105                                                            <NA>
#> 106                                                            <NA>
#> 107                                                            <NA>
#> 108                                                            <NA>
#> 109                                                            <NA>
#> 110                                                            <NA>
#> 111                                                            <NA>
#> 112                                                            <NA>
#> 113                                                            <NA>
#> 114                                                            <NA>
#> 115                                                            <NA>
#> 116                                                            <NA>
#> 117                                                            <NA>
#> 118                                                            <NA>
#> 119                                                            <NA>
#> 120                                                            <NA>
#> 121                                                            <NA>
#> 122                                                            <NA>
#> 123                                                            <NA>
#> 124                                                            <NA>
#> 125                                                            <NA>
#> 126                                                            <NA>
#> 127                                                            <NA>
#> 128                                                            <NA>
#> 129                                                            <NA>
#> 130                                                            <NA>
#> 131                                                            <NA>
#> 132                                                            <NA>
#> 133                                                            <NA>
#> 134                                                            <NA>
#> 135                                                            <NA>
#> 136                                                            <NA>
#> 137                                                            <NA>
#> 138                                                            <NA>
#> 139                                                            <NA>
#> 140                                                            <NA>
#> 141                                                            <NA>
#> 142                                                            <NA>
#> 143                                                            <NA>
#> 144                                                            <NA>
#> 145 https://wsr.nhle.com/sprites/20242025/2024020001/ev1730882.json
#> 146                                                            <NA>
#> 147                                                            <NA>
#> 148                                                            <NA>
#> 149                                                            <NA>
#> 150                                                            <NA>
#> 151                                                            <NA>
#> 152                                                            <NA>
#> 153                                                            <NA>
#> 154                                                            <NA>
#> 155                                                            <NA>
#> 156                                                            <NA>
#> 157                                                            <NA>
#> 158                                                            <NA>
#> 159                                                            <NA>
#> 160                                                            <NA>
#> 161                                                            <NA>
#> 162                                                            <NA>
#> 163                                                            <NA>
#> 164                                                            <NA>
#> 165                                                            <NA>
#> 166                                                            <NA>
#> 167                                                            <NA>
#> 168                                                            <NA>
#> 169                                                            <NA>
#> 170                                                            <NA>
#> 171                                                            <NA>
#> 172                                                            <NA>
#> 173                                                            <NA>
#> 174                                                            <NA>
#> 175                                                            <NA>
#> 176                                                            <NA>
#> 177                                                            <NA>
#> 178                                                            <NA>
#> 179                                                            <NA>
#> 180                                                            <NA>
#> 181                                                            <NA>
#> 182                                                            <NA>
#> 183                                                            <NA>
#> 184                                                            <NA>
#> 185                                                            <NA>
#> 186                                                            <NA>
#> 187                                                            <NA>
#> 188                                                            <NA>
#> 189                                                            <NA>
#> 190                                                            <NA>
#> 191                                                            <NA>
#> 192                                                            <NA>
#> 193                                                            <NA>
#> 194                                                            <NA>
#> 195                                                            <NA>
#> 196                                                            <NA>
#> 197                                                            <NA>
#> 198                                                            <NA>
#> 199                                                            <NA>
#> 200                                                            <NA>
#> 201                                                            <NA>
#> 202                                                            <NA>
#> 203                                                            <NA>
#> 204                                                            <NA>
#> 205                                                            <NA>
#> 206                                                            <NA>
#> 207                                                            <NA>
#> 208                                                            <NA>
#> 209                                                            <NA>
#> 210                                                            <NA>
#> 211                                                            <NA>
#> 212                                                            <NA>
#> 213                                                            <NA>
#> 214                                                            <NA>
#> 215                                                            <NA>
#> 216                                                            <NA>
#> 217                                                            <NA>
#> 218                                                            <NA>
#> 219                                                            <NA>
#> 220                                                            <NA>
#> 221                                                            <NA>
#> 222                                                            <NA>
#> 223                                                            <NA>
#> 224                                                            <NA>
#> 225                                                            <NA>
#> 226                                                            <NA>
#> 227                                                            <NA>
#> 228                                                            <NA>
#> 229                                                            <NA>
#> 230                                                            <NA>
#> 231                                                            <NA>
#> 232                                                            <NA>
#> 233                                                            <NA>
#> 234                                                            <NA>
#> 235                                                            <NA>
#> 236                                                            <NA>
#> 237                                                            <NA>
#> 238                                                            <NA>
#> 239                                                            <NA>
#> 240                                                            <NA>
#> 241                                                            <NA>
#> 242                                                            <NA>
#> 243                                                            <NA>
#> 244                                                            <NA>
#> 245                                                            <NA>
#> 246                                                            <NA>
#> 247                                                            <NA>
#> 248                                                            <NA>
#> 249                                                            <NA>
#> 250                                                            <NA>
#> 251                                                            <NA>
#> 252                                                            <NA>
#> 253                                                            <NA>
#> 254                                                            <NA>
#> 255                                                            <NA>
#> 256                                                            <NA>
#> 257                                                            <NA>
#> 258                                                            <NA>
#> 259                                                            <NA>
#> 260                                                            <NA>
#> 261                                                            <NA>
#> 262                                                            <NA>
#> 263                                                            <NA>
#> 264                                                            <NA>
#> 265                                                            <NA>
#> 266                                                            <NA>
#> 267                                                            <NA>
#> 268                                                            <NA>
#> 269                                                            <NA>
#> 270                                                            <NA>
#> 271                                                            <NA>
#> 272                                                            <NA>
#> 273                                                            <NA>
#> 274                                                            <NA>
#> 275                                                            <NA>
#> 276                                                            <NA>
#> 277                                                            <NA>
#> 278                                                            <NA>
#> 279                                                            <NA>
#> 280                                                            <NA>
#> 281                                                            <NA>
#> 282                                                            <NA>
#> 283                                                            <NA>
#> 284                                                            <NA>
#> 285                                                            <NA>
#> 286                                                            <NA>
#> 287                                                            <NA>
#> 288                                                            <NA>
#> 289                                                            <NA>
#> 290                                                            <NA>
#> 291                                                            <NA>
#> 292                                                            <NA>
#> 293                                                            <NA>
#> 294                                                            <NA>
#> 295                                                            <NA>
#> 296                                                            <NA>
#> 297 https://wsr.nhle.com/sprites/20242025/2024020001/ev1731224.json
#> 298                                                            <NA>
#> 299                                                            <NA>
#> 300                                                            <NA>
#> 301                                                            <NA>
#> 302                                                            <NA>
#> 303                                                            <NA>
#> 304                                                            <NA>
#> 305                                                            <NA>
#> 306                                                            <NA>
#> 307                                                            <NA>
#> 308                                                            <NA>
#> 309                                                            <NA>
#> 310                                                            <NA>
#> 311                                                            <NA>
#> 312                                                            <NA>
#> 313                                                            <NA>
#> 314                                                            <NA>
#> 315                                                            <NA>
#> 316                                                            <NA>
#> 317                                                            <NA>
#> 318                                                            <NA>
#> 319                                                            <NA>
#> 320                                                            <NA>
#> 321                                                            <NA>
#> 322                                                            <NA>
#> 323                                                            <NA>
#> 324                                                            <NA>
#> 325                                                            <NA>
#> 326                                                            <NA>
#> 327                                                            <NA>
#> 328                                                            <NA>
#> 329                                                            <NA>
#> 330                                                            <NA>
#> 331                                                            <NA>
#> 332                                                            <NA>
#> 333                                                            <NA>
#> 334                                                            <NA>
#> 335 https://wsr.nhle.com/sprites/20242025/2024020001/ev1731328.json
#> 336                                                            <NA>
#> 337                                                            <NA>
#> 338                                                            <NA>
#> 339                                                            <NA>
#> 340                                                            <NA>
#> 341                                                            <NA>
#> 342                                                            <NA>
#> 343                                                            <NA>
#> 344                                                            <NA>
#> 345                                                            <NA>
#> 346                                                            <NA>
#> 347                                                            <NA>
#> 348                                                            <NA>
#> 349                                                            <NA>
#>     periodDescriptor.number periodDescriptor.periodType
#> 1                         1                         REG
#> 2                         1                         REG
#> 3                         1                         REG
#> 4                         1                         REG
#> 5                         1                         REG
#> 6                         1                         REG
#> 7                         1                         REG
#> 8                         1                         REG
#> 9                         1                         REG
#> 10                        1                         REG
#> 11                        1                         REG
#> 12                        1                         REG
#> 13                        1                         REG
#> 14                        1                         REG
#> 15                        1                         REG
#> 16                        1                         REG
#> 17                        1                         REG
#> 18                        1                         REG
#> 19                        1                         REG
#> 20                        1                         REG
#> 21                        1                         REG
#> 22                        1                         REG
#> 23                        1                         REG
#> 24                        1                         REG
#> 25                        1                         REG
#> 26                        1                         REG
#> 27                        1                         REG
#> 28                        1                         REG
#> 29                        1                         REG
#> 30                        1                         REG
#> 31                        1                         REG
#> 32                        1                         REG
#> 33                        1                         REG
#> 34                        1                         REG
#> 35                        1                         REG
#> 36                        1                         REG
#> 37                        1                         REG
#> 38                        1                         REG
#> 39                        1                         REG
#> 40                        1                         REG
#> 41                        1                         REG
#> 42                        1                         REG
#> 43                        1                         REG
#> 44                        1                         REG
#> 45                        1                         REG
#> 46                        1                         REG
#> 47                        1                         REG
#> 48                        1                         REG
#> 49                        1                         REG
#> 50                        1                         REG
#> 51                        1                         REG
#> 52                        1                         REG
#> 53                        1                         REG
#> 54                        1                         REG
#> 55                        1                         REG
#> 56                        1                         REG
#> 57                        1                         REG
#> 58                        1                         REG
#> 59                        1                         REG
#> 60                        1                         REG
#> 61                        1                         REG
#> 62                        1                         REG
#> 63                        1                         REG
#> 64                        1                         REG
#> 65                        1                         REG
#> 66                        1                         REG
#> 67                        1                         REG
#> 68                        1                         REG
#> 69                        1                         REG
#> 70                        1                         REG
#> 71                        1                         REG
#> 72                        1                         REG
#> 73                        1                         REG
#> 74                        1                         REG
#> 75                        1                         REG
#> 76                        1                         REG
#> 77                        1                         REG
#> 78                        1                         REG
#> 79                        1                         REG
#> 80                        1                         REG
#> 81                        1                         REG
#> 82                        1                         REG
#> 83                        1                         REG
#> 84                        1                         REG
#> 85                        1                         REG
#> 86                        1                         REG
#> 87                        1                         REG
#> 88                        1                         REG
#> 89                        1                         REG
#> 90                        1                         REG
#> 91                        1                         REG
#> 92                        1                         REG
#> 93                        1                         REG
#> 94                        1                         REG
#> 95                        1                         REG
#> 96                        1                         REG
#> 97                        1                         REG
#> 98                        1                         REG
#> 99                        1                         REG
#> 100                       1                         REG
#> 101                       1                         REG
#> 102                       1                         REG
#> 103                       1                         REG
#> 104                       1                         REG
#> 105                       1                         REG
#> 106                       1                         REG
#> 107                       1                         REG
#> 108                       1                         REG
#> 109                       1                         REG
#> 110                       1                         REG
#> 111                       1                         REG
#> 112                       1                         REG
#> 113                       1                         REG
#> 114                       1                         REG
#> 115                       1                         REG
#> 116                       1                         REG
#> 117                       1                         REG
#> 118                       1                         REG
#> 119                       1                         REG
#> 120                       1                         REG
#> 121                       1                         REG
#> 122                       1                         REG
#> 123                       2                         REG
#> 124                       2                         REG
#> 125                       2                         REG
#> 126                       2                         REG
#> 127                       2                         REG
#> 128                       2                         REG
#> 129                       2                         REG
#> 130                       2                         REG
#> 131                       2                         REG
#> 132                       2                         REG
#> 133                       2                         REG
#> 134                       2                         REG
#> 135                       2                         REG
#> 136                       2                         REG
#> 137                       2                         REG
#> 138                       2                         REG
#> 139                       2                         REG
#> 140                       2                         REG
#> 141                       2                         REG
#> 142                       2                         REG
#> 143                       2                         REG
#> 144                       2                         REG
#> 145                       2                         REG
#> 146                       2                         REG
#> 147                       2                         REG
#> 148                       2                         REG
#> 149                       2                         REG
#> 150                       2                         REG
#> 151                       2                         REG
#> 152                       2                         REG
#> 153                       2                         REG
#> 154                       2                         REG
#> 155                       2                         REG
#> 156                       2                         REG
#> 157                       2                         REG
#> 158                       2                         REG
#> 159                       2                         REG
#> 160                       2                         REG
#> 161                       2                         REG
#> 162                       2                         REG
#> 163                       2                         REG
#> 164                       2                         REG
#> 165                       2                         REG
#> 166                       2                         REG
#> 167                       2                         REG
#> 168                       2                         REG
#> 169                       2                         REG
#> 170                       2                         REG
#> 171                       2                         REG
#> 172                       2                         REG
#> 173                       2                         REG
#> 174                       2                         REG
#> 175                       2                         REG
#> 176                       2                         REG
#> 177                       2                         REG
#> 178                       2                         REG
#> 179                       2                         REG
#> 180                       2                         REG
#> 181                       2                         REG
#> 182                       2                         REG
#> 183                       2                         REG
#> 184                       2                         REG
#> 185                       2                         REG
#> 186                       2                         REG
#> 187                       2                         REG
#> 188                       2                         REG
#> 189                       2                         REG
#> 190                       2                         REG
#> 191                       2                         REG
#> 192                       2                         REG
#> 193                       2                         REG
#> 194                       2                         REG
#> 195                       2                         REG
#> 196                       2                         REG
#> 197                       2                         REG
#> 198                       2                         REG
#> 199                       2                         REG
#> 200                       2                         REG
#> 201                       2                         REG
#> 202                       2                         REG
#> 203                       2                         REG
#> 204                       2                         REG
#> 205                       2                         REG
#> 206                       2                         REG
#> 207                       2                         REG
#> 208                       2                         REG
#> 209                       2                         REG
#> 210                       2                         REG
#> 211                       2                         REG
#> 212                       2                         REG
#> 213                       2                         REG
#> 214                       2                         REG
#> 215                       2                         REG
#> 216                       2                         REG
#> 217                       2                         REG
#> 218                       2                         REG
#> 219                       2                         REG
#> 220                       2                         REG
#> 221                       2                         REG
#> 222                       2                         REG
#> 223                       2                         REG
#> 224                       2                         REG
#> 225                       2                         REG
#> 226                       2                         REG
#> 227                       2                         REG
#> 228                       2                         REG
#> 229                       2                         REG
#> 230                       2                         REG
#> 231                       2                         REG
#> 232                       2                         REG
#> 233                       2                         REG
#> 234                       2                         REG
#> 235                       2                         REG
#> 236                       2                         REG
#> 237                       3                         REG
#> 238                       3                         REG
#> 239                       3                         REG
#> 240                       3                         REG
#> 241                       3                         REG
#> 242                       3                         REG
#> 243                       3                         REG
#> 244                       3                         REG
#> 245                       3                         REG
#> 246                       3                         REG
#> 247                       3                         REG
#> 248                       3                         REG
#> 249                       3                         REG
#> 250                       3                         REG
#> 251                       3                         REG
#> 252                       3                         REG
#> 253                       3                         REG
#> 254                       3                         REG
#> 255                       3                         REG
#> 256                       3                         REG
#> 257                       3                         REG
#> 258                       3                         REG
#> 259                       3                         REG
#> 260                       3                         REG
#> 261                       3                         REG
#> 262                       3                         REG
#> 263                       3                         REG
#> 264                       3                         REG
#> 265                       3                         REG
#> 266                       3                         REG
#> 267                       3                         REG
#> 268                       3                         REG
#> 269                       3                         REG
#> 270                       3                         REG
#> 271                       3                         REG
#> 272                       3                         REG
#> 273                       3                         REG
#> 274                       3                         REG
#> 275                       3                         REG
#> 276                       3                         REG
#> 277                       3                         REG
#> 278                       3                         REG
#> 279                       3                         REG
#> 280                       3                         REG
#> 281                       3                         REG
#> 282                       3                         REG
#> 283                       3                         REG
#> 284                       3                         REG
#> 285                       3                         REG
#> 286                       3                         REG
#> 287                       3                         REG
#> 288                       3                         REG
#> 289                       3                         REG
#> 290                       3                         REG
#> 291                       3                         REG
#> 292                       3                         REG
#> 293                       3                         REG
#> 294                       3                         REG
#> 295                       3                         REG
#> 296                       3                         REG
#> 297                       3                         REG
#> 298                       3                         REG
#> 299                       3                         REG
#> 300                       3                         REG
#> 301                       3                         REG
#> 302                       3                         REG
#> 303                       3                         REG
#> 304                       3                         REG
#> 305                       3                         REG
#> 306                       3                         REG
#> 307                       3                         REG
#> 308                       3                         REG
#> 309                       3                         REG
#> 310                       3                         REG
#> 311                       3                         REG
#> 312                       3                         REG
#> 313                       3                         REG
#> 314                       3                         REG
#> 315                       3                         REG
#> 316                       3                         REG
#> 317                       3                         REG
#> 318                       3                         REG
#> 319                       3                         REG
#> 320                       3                         REG
#> 321                       3                         REG
#> 322                       3                         REG
#> 323                       3                         REG
#> 324                       3                         REG
#> 325                       3                         REG
#> 326                       3                         REG
#> 327                       3                         REG
#> 328                       3                         REG
#> 329                       3                         REG
#> 330                       3                         REG
#> 331                       3                         REG
#> 332                       3                         REG
#> 333                       3                         REG
#> 334                       3                         REG
#> 335                       3                         REG
#> 336                       3                         REG
#> 337                       3                         REG
#> 338                       3                         REG
#> 339                       3                         REG
#> 340                       3                         REG
#> 341                       3                         REG
#> 342                       3                         REG
#> 343                       3                         REG
#> 344                       3                         REG
#> 345                       3                         REG
#> 346                       3                         REG
#> 347                       3                         REG
#> 348                       3                         REG
#> 349                       3                         REG
#>     periodDescriptor.maxRegulationPeriods details.eventOwnerTeamId
#> 1                                       3                       NA
#> 2                                       3                        1
#> 3                                       3                        7
#> 4                                       3                        1
#> 5                                       3                        1
#> 6                                       3                        1
#> 7                                       3                        7
#> 8                                       3                        1
#> 9                                       3                        1
#> 10                                      3                       NA
#> 11                                      3                        7
#> 12                                      3                        7
#> 13                                      3                        7
#> 14                                      3                        1
#> 15                                      3                       NA
#> 16                                      3                        7
#> 17                                      3                        7
#> 18                                      3                        1
#> 19                                      3                        1
#> 20                                      3                        1
#> 21                                      3                        1
#> 22                                      3                        1
#> 23                                      3                        1
#> 24                                      3                        7
#> 25                                      3                        1
#> 26                                      3                        1
#> 27                                      3                        7
#> 28                                      3                        7
#> 29                                      3                        1
#> 30                                      3                        7
#> 31                                      3                        1
#> 32                                      3                        7
#> 33                                      3                        7
#> 34                                      3                        1
#> 35                                      3                       NA
#> 36                                      3                        7
#> 37                                      3                        1
#> 38                                      3                        7
#> 39                                      3                        7
#> 40                                      3                       NA
#> 41                                      3                        7
#> 42                                      3                        7
#> 43                                      3                        7
#> 44                                      3                        7
#> 45                                      3                        7
#> 46                                      3                        1
#> 47                                      3                        1
#> 48                                      3                        7
#> 49                                      3                       NA
#> 50                                      3                        7
#> 51                                      3                        7
#> 52                                      3                        7
#> 53                                      3                        7
#> 54                                      3                        1
#> 55                                      3                        7
#> 56                                      3                        1
#> 57                                      3                        1
#> 58                                      3                        1
#> 59                                      3                        1
#> 60                                      3                        1
#> 61                                      3                        1
#> 62                                      3                        1
#> 63                                      3                        1
#> 64                                      3                        1
#> 65                                      3                       NA
#> 66                                      3                        1
#> 67                                      3                        1
#> 68                                      3                        1
#> 69                                      3                       NA
#> 70                                      3                        7
#> 71                                      3                        1
#> 72                                      3                        7
#> 73                                      3                        1
#> 74                                      3                        1
#> 75                                      3                        1
#> 76                                      3                        7
#> 77                                      3                        7
#> 78                                      3                        1
#> 79                                      3                        1
#> 80                                      3                        1
#> 81                                      3                        1
#> 82                                      3                        1
#> 83                                      3                        7
#> 84                                      3                        7
#> 85                                      3                        7
#> 86                                      3                        1
#> 87                                      3                       NA
#> 88                                      3                        7
#> 89                                      3                        1
#> 90                                      3                        7
#> 91                                      3                       NA
#> 92                                      3                        1
#> 93                                      3                        1
#> 94                                      3                        7
#> 95                                      3                       NA
#> 96                                      3                        7
#> 97                                      3                        7
#> 98                                      3                       NA
#> 99                                      3                        7
#> 100                                     3                        1
#> 101                                     3                        1
#> 102                                     3                        7
#> 103                                     3                        1
#> 104                                     3                        7
#> 105                                     3                        7
#> 106                                     3                        7
#> 107                                     3                        7
#> 108                                     3                        7
#> 109                                     3                        1
#> 110                                     3                       NA
#> 111                                     3                        7
#> 112                                     3                       NA
#> 113                                     3                        7
#> 114                                     3                        1
#> 115                                     3                        1
#> 116                                     3                        1
#> 117                                     3                        1
#> 118                                     3                        1
#> 119                                     3                        1
#> 120                                     3                        1
#> 121                                     3                        7
#> 122                                     3                       NA
#> 123                                     3                       NA
#> 124                                     3                        7
#> 125                                     3                       NA
#> 126                                     3                        1
#> 127                                     3                        1
#> 128                                     3                        1
#> 129                                     3                        1
#> 130                                     3                        7
#> 131                                     3                        7
#> 132                                     3                        7
#> 133                                     3                       NA
#> 134                                     3                        7
#> 135                                     3                        1
#> 136                                     3                        1
#> 137                                     3                        7
#> 138                                     3                        7
#> 139                                     3                        7
#> 140                                     3                        1
#> 141                                     3                        1
#> 142                                     3                        7
#> 143                                     3                        1
#> 144                                     3                        1
#> 145                                     3                        1
#> 146                                     3                        7
#> 147                                     3                        1
#> 148                                     3                        1
#> 149                                     3                        1
#> 150                                     3                        7
#> 151                                     3                        7
#> 152                                     3                       NA
#> 153                                     3                        7
#> 154                                     3                        1
#> 155                                     3                        7
#> 156                                     3                        1
#> 157                                     3                        1
#> 158                                     3                        7
#> 159                                     3                        1
#> 160                                     3                       NA
#> 161                                     3                        1
#> 162                                     3                       NA
#> 163                                     3                        7
#> 164                                     3                        7
#> 165                                     3                       NA
#> 166                                     3                        1
#> 167                                     3                        7
#> 168                                     3                        7
#> 169                                     3                        7
#> 170                                     3                        7
#> 171                                     3                        7
#> 172                                     3                       NA
#> 173                                     3                        7
#> 174                                     3                        7
#> 175                                     3                        1
#> 176                                     3                        1
#> 177                                     3                        1
#> 178                                     3                        7
#> 179                                     3                        7
#> 180                                     3                        1
#> 181                                     3                        1
#> 182                                     3                        7
#> 183                                     3                        7
#> 184                                     3                        1
#> 185                                     3                       NA
#> 186                                     3                        1
#> 187                                     3                        1
#> 188                                     3                        7
#> 189                                     3                        1
#> 190                                     3                        7
#> 191                                     3                        7
#> 192                                     3                        7
#> 193                                     3                        7
#> 194                                     3                        7
#> 195                                     3                       NA
#> 196                                     3                        1
#> 197                                     3                        1
#> 198                                     3                        7
#> 199                                     3                        1
#> 200                                     3                        7
#> 201                                     3                        7
#> 202                                     3                        1
#> 203                                     3                        7
#> 204                                     3                        7
#> 205                                     3                       NA
#> 206                                     3                        1
#> 207                                     3                        7
#> 208                                     3                        1
#> 209                                     3                        1
#> 210                                     3                        1
#> 211                                     3                       NA
#> 212                                     3                        7
#> 213                                     3                        1
#> 214                                     3                        1
#> 215                                     3                        1
#> 216                                     3                        1
#> 217                                     3                        7
#> 218                                     3                        7
#> 219                                     3                       NA
#> 220                                     3                        1
#> 221                                     3                        7
#> 222                                     3                        7
#> 223                                     3                        7
#> 224                                     3                        1
#> 225                                     3                       NA
#> 226                                     3                        7
#> 227                                     3                        7
#> 228                                     3                        7
#> 229                                     3                        1
#> 230                                     3                        1
#> 231                                     3                        1
#> 232                                     3                       NA
#> 233                                     3                        7
#> 234                                     3                       NA
#> 235                                     3                        1
#> 236                                     3                       NA
#> 237                                     3                       NA
#> 238                                     3                        1
#> 239                                     3                        1
#> 240                                     3                       NA
#> 241                                     3                        1
#> 242                                     3                        7
#> 243                                     3                        7
#> 244                                     3                       NA
#> 245                                     3                        7
#> 246                                     3                        1
#> 247                                     3                        1
#> 248                                     3                        7
#> 249                                     3                       NA
#> 250                                     3                        1
#> 251                                     3                        7
#> 252                                     3                        7
#> 253                                     3                        1
#> 254                                     3                        7
#> 255                                     3                        7
#> 256                                     3                        7
#> 257                                     3                        7
#> 258                                     3                        1
#> 259                                     3                        7
#> 260                                     3                        7
#> 261                                     3                        1
#> 262                                     3                        1
#> 263                                     3                        1
#> 264                                     3                        7
#> 265                                     3                        7
#> 266                                     3                       NA
#> 267                                     3                        7
#> 268                                     3                        7
#> 269                                     3                        1
#> 270                                     3                        7
#> 271                                     3                       NA
#> 272                                     3                        1
#> 273                                     3                        1
#> 274                                     3                       NA
#> 275                                     3                        7
#> 276                                     3                        7
#> 277                                     3                        1
#> 278                                     3                        7
#> 279                                     3                       NA
#> 280                                     3                        1
#> 281                                     3                        1
#> 282                                     3                       NA
#> 283                                     3                        7
#> 284                                     3                        7
#> 285                                     3                        1
#> 286                                     3                        1
#> 287                                     3                        7
#> 288                                     3                        1
#> 289                                     3                        7
#> 290                                     3                        7
#> 291                                     3                        1
#> 292                                     3                        1
#> 293                                     3                        1
#> 294                                     3                        7
#> 295                                     3                        7
#> 296                                     3                        1
#> 297                                     3                        7
#> 298                                     3                        1
#> 299                                     3                        7
#> 300                                     3                       NA
#> 301                                     3                        7
#> 302                                     3                        1
#> 303                                     3                        7
#> 304                                     3                       NA
#> 305                                     3                        7
#> 306                                     3                        7
#> 307                                     3                        7
#> 308                                     3                        7
#> 309                                     3                        7
#> 310                                     3                       NA
#> 311                                     3                        7
#> 312                                     3                       NA
#> 313                                     3                        7
#> 314                                     3                        1
#> 315                                     3                        7
#> 316                                     3                        1
#> 317                                     3                        7
#> 318                                     3                       NA
#> 319                                     3                        1
#> 320                                     3                       NA
#> 321                                     3                        7
#> 322                                     3                        7
#> 323                                     3                       NA
#> 324                                     3                        7
#> 325                                     3                        7
#> 326                                     3                        7
#> 327                                     3                        7
#> 328                                     3                        7
#> 329                                     3                        7
#> 330                                     3                       NA
#> 331                                     3                        7
#> 332                                     3                        1
#> 333                                     3                        7
#> 334                                     3                        7
#> 335                                     3                        1
#> 336                                     3                        7
#> 337                                     3                        7
#> 338                                     3                        7
#> 339                                     3                       NA
#> 340                                     3                        1
#> 341                                     3                        1
#> 342                                     3                        1
#> 343                                     3                        1
#> 344                                     3                       NA
#> 345                                     3                        7
#> 346                                     3                        7
#> 347                                     3                        7
#> 348                                     3                       NA
#> 349                                     3                       NA
#>     details.losingPlayerId details.winningPlayerId details.xCoord
#> 1                       NA                      NA             NA
#> 2                  8478043                 8480002              0
#> 3                       NA                      NA             68
#> 4                       NA                      NA             56
#> 5                       NA                      NA            -73
#> 6                       NA                      NA             71
#> 7                       NA                      NA             68
#> 8                       NA                      NA             47
#> 9                       NA                      NA             58
#> 10                      NA                      NA             NA
#> 11                 8481559                 8479420            -69
#> 12                      NA                      NA            -68
#> 13                      NA                      NA            -40
#> 14                      NA                      NA            -74
#> 15                      NA                      NA             NA
#> 16                 8475287                 8477949            -69
#> 17                      NA                      NA            -36
#> 18                      NA                      NA            -67
#> 19                      NA                      NA              1
#> 20                 8477949                 8480002            -69
#> 21                      NA                      NA            -80
#> 22                      NA                      NA            -41
#> 23                      NA                      NA            -91
#> 24                      NA                      NA            -10
#> 25                      NA                      NA             79
#> 26                      NA                      NA             96
#> 27                      NA                      NA            -86
#> 28                      NA                      NA            -53
#> 29                      NA                      NA             -5
#> 30                      NA                      NA            -67
#> 31                      NA                      NA            -82
#> 32                      NA                      NA            -96
#> 33                      NA                      NA            -58
#> 34                      NA                      NA            -97
#> 35                      NA                      NA             NA
#> 36                 8480002                 8481528            -20
#> 37                      NA                      NA            -34
#> 38                      NA                      NA            -92
#> 39                      NA                      NA            -46
#> 40                      NA                      NA             NA
#> 41                 8477508                 8478043            -69
#> 42                      NA                      NA            -73
#> 43                      NA                      NA            -95
#> 44                      NA                      NA            -96
#> 45                      NA                      NA            -97
#> 46                      NA                      NA            -97
#> 47                      NA                      NA            -77
#> 48                      NA                      NA            -82
#> 49                      NA                      NA             NA
#> 50                 8480002                 8481528            -69
#> 51                      NA                      NA            -94
#> 52                      NA                      NA             98
#> 53                      NA                      NA             97
#> 54                      NA                      NA             34
#> 55                      NA                      NA             85
#> 56                      NA                      NA             85
#> 57                      NA                      NA             71
#> 58                 8477949                 8478414              0
#> 59                      NA                      NA             50
#> 60                      NA                      NA             57
#> 61                      NA                      NA             82
#> 62                      NA                      NA             97
#> 63                      NA                      NA             87
#> 64                      NA                      NA             43
#> 65                      NA                      NA             NA
#> 66                 8478043                 8480002             69
#> 67                      NA                      NA             52
#> 68                      NA                      NA             32
#> 69                      NA                      NA             NA
#> 70                 8481559                 8479420            -20
#> 71                      NA                      NA             74
#> 72                      NA                      NA             64
#> 73                 8480802                 8480002             69
#> 74                      NA                      NA             59
#> 75                      NA                      NA             65
#> 76                      NA                      NA             97
#> 77                      NA                      NA             96
#> 78                      NA                      NA             59
#> 79                      NA                      NA             85
#> 80                      NA                      NA             74
#> 81                      NA                      NA             87
#> 82                      NA                      NA            -83
#> 83                      NA                      NA            -70
#> 84                      NA                      NA            -73
#> 85                      NA                      NA            -75
#> 86                      NA                      NA             70
#> 87                      NA                      NA             NA
#> 88                 8475287                 8480802             69
#> 89                      NA                      NA             80
#> 90                      NA                      NA            -86
#> 91                      NA                      NA             NA
#> 92                 8477949                 8480002            -69
#> 93                      NA                      NA             38
#> 94                 8481559                 8481528              0
#> 95                      NA                      NA             NA
#> 96                 8481559                 8481528              0
#> 97                      NA                      NA            -60
#> 98                      NA                      NA             NA
#> 99                 8477508                 8481528            -69
#> 100                     NA                      NA             95
#> 101                     NA                      NA             -7
#> 102                     NA                      NA            -65
#> 103                     NA                      NA            -60
#> 104                     NA                      NA            -78
#> 105                     NA                      NA            -76
#> 106                     NA                      NA            -76
#> 107                     NA                      NA             NA
#> 108                     NA                      NA            -97
#> 109                8480802                 8475287             69
#> 110                     NA                      NA             NA
#> 111                8480002                 8477949             69
#> 112                     NA                      NA             NA
#> 113                8480002                 8477949             20
#> 114                     NA                      NA             78
#> 115                     NA                      NA             39
#> 116                     NA                      NA             73
#> 117                     NA                      NA             51
#> 118                     NA                      NA             55
#> 119                     NA                      NA             93
#> 120                     NA                      NA             57
#> 121                     NA                      NA            -51
#> 122                     NA                      NA             NA
#> 123                     NA                      NA             NA
#> 124                8480002                 8479420              0
#> 125                     NA                      NA             NA
#> 126                8479420                 8480002             69
#> 127                     NA                      NA            -56
#> 128                     NA                      NA            -36
#> 129                     NA                      NA             97
#> 130                     NA                      NA             85
#> 131                     NA                      NA             69
#> 132                     NA                      NA            -81
#> 133                     NA                      NA             NA
#> 134                8475287                 8480802            -69
#> 135                     NA                      NA            -69
#> 136                     NA                      NA             89
#> 137                     NA                      NA             81
#> 138                     NA                      NA             81
#> 139                     NA                      NA             65
#> 140                     NA                      NA             67
#> 141                     NA                      NA             46
#> 142                     NA                      NA            -98
#> 143                     NA                      NA            -41
#> 144                     NA                      NA            -95
#> 145                     NA                      NA            -50
#> 146                8480002                 8481528              0
#> 147                     NA                      NA            -77
#> 148                     NA                      NA            -62
#> 149                     NA                      NA            -95
#> 150                     NA                      NA             97
#> 151                     NA                      NA             55
#> 152                     NA                      NA             NA
#> 153                8475287                 8479420             69
#> 154                     NA                      NA            -77
#> 155                     NA                      NA             95
#> 156                     NA                      NA             95
#> 157                     NA                      NA             11
#> 158                     NA                      NA             48
#> 159                     NA                      NA             73
#> 160                     NA                      NA             NA
#> 161                8477949                 8480002             69
#> 162                     NA                      NA             NA
#> 163                8480002                 8481528              0
#> 164                     NA                      NA             52
#> 165                     NA                      NA             NA
#> 166                8484145                 8475287             69
#> 167                     NA                      NA             57
#> 168                     NA                      NA             63
#> 169                     NA                      NA             60
#> 170                     NA                      NA             73
#> 171                     NA                      NA             70
#> 172                     NA                      NA             NA
#> 173                8477508                 8479420             69
#> 174                     NA                      NA             54
#> 175                     NA                      NA             33
#> 176                     NA                      NA             82
#> 177                     NA                      NA            -76
#> 178                     NA                      NA            -98
#> 179                     NA                      NA            -76
#> 180                     NA                      NA            -78
#> 181                     NA                      NA            -57
#> 182                     NA                      NA            -70
#> 183                     NA                      NA            -30
#> 184                     NA                      NA            -94
#> 185                     NA                      NA             NA
#> 186                8478043                 8480002             20
#> 187                     NA                      NA            -63
#> 188                     NA                      NA            -84
#> 189                     NA                      NA             96
#> 190                     NA                      NA             96
#> 191                     NA                      NA             45
#> 192                     NA                      NA             76
#> 193                     NA                      NA             73
#> 194                     NA                      NA            -58
#> 195                     NA                      NA             NA
#> 196                8479420                 8481559             69
#> 197                     NA                      NA             87
#> 198                     NA                      NA             38
#> 199                     NA                      NA             58
#> 200                     NA                      NA             61
#> 201                     NA                      NA             28
#> 202                     NA                      NA             58
#> 203                     NA                      NA             61
#> 204                     NA                      NA             81
#> 205                     NA                      NA             NA
#> 206                8481528                 8480002             20
#> 207                     NA                      NA             47
#> 208                     NA                      NA            -29
#> 209                     NA                      NA            -72
#> 210                     NA                      NA            -40
#> 211                     NA                      NA             NA
#> 212                8475287                 8480802            -69
#> 213                     NA                      NA             NA
#> 214                     NA                      NA             95
#> 215                8477949                 8482110             69
#> 216                     NA                      NA             -6
#> 217                     NA                      NA             73
#> 218                     NA                      NA             45
#> 219                     NA                      NA             NA
#> 220                8481528                 8475287             69
#> 221                     NA                      NA             94
#> 222                     NA                      NA             76
#> 223                     NA                      NA             75
#> 224                     NA                      NA            -78
#> 225                     NA                      NA             NA
#> 226                8480002                 8479420             69
#> 227                     NA                      NA             35
#> 228                     NA                      NA             92
#> 229                     NA                      NA            -61
#> 230                     NA                      NA            -84
#> 231                     NA                      NA            -60
#> 232                     NA                      NA             NA
#> 233                8477508                 8481528             20
#> 234                     NA                      NA             NA
#> 235                8481528                 8481032            -20
#> 236                     NA                      NA             NA
#> 237                     NA                      NA             NA
#> 238                8479420                 8480002              0
#> 239                     NA                      NA            -28
#> 240                     NA                      NA             NA
#> 241                8479420                 8480002            -69
#> 242                     NA                      NA             11
#> 243                     NA                      NA            -43
#> 244                     NA                      NA             NA
#> 245                8481559                 8481528            -69
#> 246                     NA                      NA            -17
#> 247                     NA                      NA            -63
#> 248                     NA                      NA            -66
#> 249                     NA                      NA             NA
#> 250                8480802                 8475287             69
#> 251                     NA                      NA             97
#> 252                     NA                      NA            -78
#> 253                     NA                      NA            -71
#> 254                     NA                      NA            -85
#> 255                     NA                      NA            -58
#> 256                     NA                      NA            -69
#> 257                     NA                      NA             78
#> 258                     NA                      NA             85
#> 259                     NA                      NA            -38
#> 260                     NA                      NA             27
#> 261                     NA                      NA             92
#> 262                     NA                      NA            -63
#> 263                8481528                 8480002            -69
#> 264                     NA                      NA            -83
#> 265                     NA                      NA            -82
#> 266                     NA                      NA             NA
#> 267                8477508                 8477949            -69
#> 268                     NA                      NA            -77
#> 269                     NA                      NA            -69
#> 270                     NA                      NA            -76
#> 271                     NA                      NA             NA
#> 272                8477949                 8480002            -69
#> 273                     NA                      NA             34
#> 274                     NA                      NA             NA
#> 275                8481559                 8480802             69
#> 276                     NA                      NA            -47
#> 277                     NA                      NA            -96
#> 278                     NA                      NA            -55
#> 279                     NA                      NA             NA
#> 280                8481528                 8480002            -69
#> 281                     NA                      NA             63
#> 282                     NA                      NA             NA
#> 283                8480002                 8481528             69
#> 284                     NA                      NA             88
#> 285                     NA                      NA             56
#> 286                     NA                      NA            -13
#> 287                     NA                      NA            -76
#> 288                     NA                      NA            -69
#> 289                     NA                      NA            -57
#> 290                     NA                      NA            -34
#> 291                     NA                      NA             67
#> 292                     NA                      NA             66
#> 293                     NA                      NA             76
#> 294                     NA                      NA            -47
#> 295                     NA                      NA            -81
#> 296                     NA                      NA            -97
#> 297                     NA                      NA            -77
#> 298                8480802                 8475287              0
#> 299                     NA                      NA             20
#> 300                     NA                      NA             NA
#> 301                8481559                 8480802             69
#> 302                     NA                      NA            -31
#> 303                     NA                      NA            -77
#> 304                     NA                      NA             NA
#> 305                8475287                 8479420            -69
#> 306                     NA                      NA            -51
#> 307                     NA                      NA             56
#> 308                     NA                      NA             -6
#> 309                     NA                      NA            -36
#> 310                     NA                      NA             NA
#> 311                8481559                 8480802             20
#> 312                     NA                      NA             NA
#> 313                8481559                 8480802             69
#> 314                     NA                      NA             59
#> 315                     NA                      NA            -69
#> 316                     NA                      NA            -38
#> 317                     NA                      NA            -32
#> 318                     NA                      NA             NA
#> 319                8477949                 8480002            -69
#> 320                     NA                      NA             NA
#> 321                8480002                 8480802            -69
#> 322                     NA                      NA            -42
#> 323                     NA                      NA             NA
#> 324                8480002                 8481528            -69
#> 325                     NA                      NA            -34
#> 326                     NA                      NA            -86
#> 327                     NA                      NA            -87
#> 328                     NA                      NA            -88
#> 329                     NA                      NA            -62
#> 330                     NA                      NA             NA
#> 331                8480002                 8479420            -69
#> 332                     NA                      NA             60
#> 333                     NA                      NA            -42
#> 334                     NA                      NA            -45
#> 335                     NA                      NA             78
#> 336                8480002                 8479420              0
#> 337                     NA                      NA            -81
#> 338                     NA                      NA            -27
#> 339                     NA                      NA             NA
#> 340                8478043                 8475287            -69
#> 341                     NA                      NA            -56
#> 342                     NA                      NA             80
#> 343                     NA                      NA             74
#> 344                     NA                      NA             NA
#> 345                8477508                 8479420            -69
#> 346                     NA                      NA            -55
#> 347                     NA                      NA            -41
#> 348                     NA                      NA             NA
#> 349                     NA                      NA             NA
#>     details.yCoord details.zoneCode details.hittingPlayerId
#> 1               NA             <NA>                      NA
#> 2                0                N                      NA
#> 3              -40                N                 8479359
#> 4              -39                O                      NA
#> 5                8                D                      NA
#> 6              -28                O                      NA
#> 7               40                N                 8481524
#> 8              -23                O                      NA
#> 9               21                D                      NA
#> 10              NA             <NA>                      NA
#> 11              22                O                      NA
#> 12              11                D                      NA
#> 13             -30                O                      NA
#> 14             -39                N                 8479407
#> 15              NA             <NA>                      NA
#> 16             -22                O                      NA
#> 17              15                O                      NA
#> 18              41                N                 8475287
#> 19             -37                N                      NA
#> 20              22                D                      NA
#> 21             -39                N                 8475455
#> 22              38                D                 8477508
#> 23             -31                N                 8478399
#> 24              12                N                      NA
#> 25               2                O                      NA
#> 26              11                O                      NA
#> 27              17                O                      NA
#> 28             -30                O                      NA
#> 29              40                N                      NA
#> 30              26                O                      NA
#> 31              38                N                 8483495
#> 32              -4                N                 8478413
#> 33             -16                O                      NA
#> 34               4                D                      NA
#> 35              NA             <NA>                      NA
#> 36             -22                N                      NA
#> 37             -40                N                 8483429
#> 38              26                O                      NA
#> 39              -6                D                      NA
#> 40              NA             <NA>                      NA
#> 41             -22                O                      NA
#> 42              39                N                 8479359
#> 43              17                N                 8477979
#> 44             -22                N                 8478043
#> 45              17                N                 8477979
#> 46              13                D                      NA
#> 47             -15                D                      NA
#> 48               5                O                      NA
#> 49              NA             <NA>                      NA
#> 50              22                O                      NA
#> 51             -29                N                 8481528
#> 52             -11                D                 8481524
#> 53              -7                N                 8482671
#> 54              22                O                      NA
#> 55              36                N                 8477365
#> 56              36                N                 8476474
#> 57             -12                O                      NA
#> 58               0                N                      NA
#> 59             -24                D                      NA
#> 60              19                D                      NA
#> 61              14                O                      NA
#> 62              -7                N                 8479407
#> 63              -7                O                      NA
#> 64              28                O                      NA
#> 65              NA             <NA>                      NA
#> 66              22                O                      NA
#> 67              -7                D                      NA
#> 68             -19                O                      NA
#> 69              NA             <NA>                      NA
#> 70             -22                N                      NA
#> 71             -10                D                      NA
#> 72               1                D                      NA
#> 73              22                O                      NA
#> 74             -11                D                      NA
#> 75             -10                D                      NA
#> 76               2                D                      NA
#> 77               0                N                 8477365
#> 78              19                O                      NA
#> 79               7                D                      NA
#> 80             -14                D                      NA
#> 81              -1                D                      NA
#> 82              37                N                 8483429
#> 83              40                O                      NA
#> 84              -6                D                      NA
#> 85              -1                D                      NA
#> 86               0                O                      NA
#> 87              NA             <NA>                      NA
#> 88              22                D                      NA
#> 89              38                N                 8475287
#> 90               6                O                      NA
#> 91              NA             <NA>                      NA
#> 92              22                D                      NA
#> 93               1                O                      NA
#> 94               0                N                      NA
#> 95              NA             <NA>                      NA
#> 96               0                N                      NA
#> 97              -3                O                      NA
#> 98              NA             <NA>                      NA
#> 99             -22                O                      NA
#> 100             24                N                 8477508
#> 101             40                N                 8481032
#> 102            -34                O                      NA
#> 103            -40                D                 8478414
#> 104              1                D                      NA
#> 105             -3                O                      NA
#> 106             40                N                 8479359
#> 107             NA             <NA>                      NA
#> 108              1                O                      NA
#> 109             22                O                      NA
#> 110             NA             <NA>                      NA
#> 111             22                D                      NA
#> 112             NA             <NA>                      NA
#> 113            -22                N                      NA
#> 114            -16                O                      NA
#> 115            -32                D                      NA
#> 116             -6                D                      NA
#> 117             27                O                      NA
#> 118             25                D                      NA
#> 119            -25                N                 8477508
#> 120            -37                D                      NA
#> 121            -25                O                      NA
#> 122             NA             <NA>                      NA
#> 123             NA             <NA>                      NA
#> 124              0                N                      NA
#> 125             NA             <NA>                      NA
#> 126            -22                D                      NA
#> 127             25                O                      NA
#> 128            -29                O                      NA
#> 129             17                N                 8475455
#> 130            -33                D                      NA
#> 131              7                O                      NA
#> 132             -4                N                 8478413
#> 133             NA             <NA>                      NA
#> 134            -22                D                      NA
#> 135             24                O                      NA
#> 136            -15                D                      NA
#> 137             -3                O                      NA
#> 138             -5                O                      NA
#> 139             36                O                      NA
#> 140             39                N                 8475287
#> 141             39                N                 8476474
#> 142             17                D                 8480839
#> 143             -3                O                      NA
#> 144             -9                O                      NA
#> 145              3                O                      NA
#> 146              0                N                      NA
#> 147            -12                O                      NA
#> 148             12                D                      NA
#> 149            -26                N                 8479407
#> 150              7                N                 8479359
#> 151              5                O                      NA
#> 152             NA             <NA>                      NA
#> 153             22                O                      NA
#> 154             40                O                      NA
#> 155            -18                N                 8482175
#> 156            -22                D                      NA
#> 157            -38                N                      NA
#> 158              6                O                      NA
#> 159             40                D                      NA
#> 160             NA             <NA>                      NA
#> 161            -22                D                      NA
#> 162             NA             <NA>                      NA
#> 163              0                N                      NA
#> 164            -10                O                      NA
#> 165             NA             <NA>                      NA
#> 166             22                D                      NA
#> 167            -18                O                      NA
#> 168             25                O                      NA
#> 169            -19                O                      NA
#> 170              5                O                      NA
#> 171            -12                O                      NA
#> 172             NA             <NA>                      NA
#> 173            -22                O                      NA
#> 174            -17                O                      NA
#> 175            -38                N                 8478399
#> 176            -38                D                 8476462
#> 177              4                O                      NA
#> 178             -2                D                      NA
#> 179             14                D                      NA
#> 180              0                O                      NA
#> 181             27                O                      NA
#> 182              5                D                      NA
#> 183            -39                N                 8479359
#> 184            -19                N                 8478414
#> 185             NA             <NA>                      NA
#> 186            -22                N                      NA
#> 187             13                D                      NA
#> 188            -35                N                 8477979
#> 189              4                N                 8478399
#> 190             17                N                 8479359
#> 191            -27                O                      NA
#> 192             12                D                      NA
#> 193             -8                D                      NA
#> 194             39                D                 8480035
#> 195             NA             <NA>                      NA
#> 196            -22                D                      NA
#> 197            -28                D                      NA
#> 198              7                O                      NA
#> 199            -35                D                      NA
#> 200            -21                D                      NA
#> 201             31                O                      NA
#> 202            -39                D                 8478414
#> 203             33                D                      NA
#> 204              5                D                      NA
#> 205             NA             <NA>                      NA
#> 206             22                N                      NA
#> 207            -14                O                      NA
#> 208            -16                O                      NA
#> 209             24                O                      NA
#> 210             14                O                      NA
#> 211             NA             <NA>                      NA
#> 212             22                D                      NA
#> 213             NA             <NA>                      NA
#> 214             25                D                      NA
#> 215            -22                D                      NA
#> 216              4                N                      NA
#> 217            -18                O                      NA
#> 218             10                O                      NA
#> 219             NA             <NA>                      NA
#> 220             22                D                      NA
#> 221              5                O                      NA
#> 222              6                O                      NA
#> 223             -1                D                      NA
#> 224             11                D                      NA
#> 225             NA             <NA>                      NA
#> 226            -22                O                      NA
#> 227            -35                O                      NA
#> 228            -29                N                 8477949
#> 229              3                O                      NA
#> 230            -35                N                 8476292
#> 231            -37                D                      NA
#> 232             NA             <NA>                      NA
#> 233             22                N                      NA
#> 234             NA             <NA>                      NA
#> 235            -22                N                      NA
#> 236             NA             <NA>                      NA
#> 237             NA             <NA>                      NA
#> 238              0                N                      NA
#> 239            -32                D                      NA
#> 240             NA             <NA>                      NA
#> 241             22                D                      NA
#> 242            -25                N                 8480035
#> 243            -25                O                      NA
#> 244             NA             <NA>                      NA
#> 245            -22                O                      NA
#> 246            -40                N                 8479407
#> 247            -39                N                 8478414
#> 248             18                O                      NA
#> 249             NA             <NA>                      NA
#> 250             22                O                      NA
#> 251             14                D                 8480807
#> 252             40                N                 8477949
#> 253             39                D                      NA
#> 254             16                O                      NA
#> 255            -37                N                 8477949
#> 256             -2                D                      NA
#> 257             38                D                      NA
#> 258             -2                D                      NA
#> 259            -22                O                      NA
#> 260            -35                D                      NA
#> 261             30                O                      NA
#> 262              5                D                      NA
#> 263             22                D                      NA
#> 264            -13                O                      NA
#> 265             -4                O                      NA
#> 266             NA             <NA>                      NA
#> 267             22                O                      NA
#> 268              4                D                      NA
#> 269            -39                N                 8476292
#> 270              2                D                      NA
#> 271             NA             <NA>                      NA
#> 272             22                D                      NA
#> 273             -7                O                      NA
#> 274             NA             <NA>                      NA
#> 275            -22                D                      NA
#> 276             -5                O                      NA
#> 277             24                D                      NA
#> 278             39                O                      NA
#> 279             NA             <NA>                      NA
#> 280            -22                D                      NA
#> 281             29                O                      NA
#> 282             NA             <NA>                      NA
#> 283             22                D                      NA
#> 284            -32                D                      NA
#> 285            -40                O                      NA
#> 286             35                N                      NA
#> 287             13                O                      NA
#> 288            -39                N                 8481032
#> 289            -39                N                 8477949
#> 290            -39                O                      NA
#> 291            -15                O                      NA
#> 292             40                O                 8481032
#> 293             40                O                 8479414
#> 294            -24                O                      NA
#> 295             -7                O                      NA
#> 296              8                N                 8475455
#> 297             12                O                      NA
#> 298              0                N                      NA
#> 299             39                N                 8477365
#> 300             NA             <NA>                      NA
#> 301             22                D                      NA
#> 302            -28                D                 8478414
#> 303             33                O                      NA
#> 304             NA             <NA>                      NA
#> 305             22                O                      NA
#> 306             38                O                      NA
#> 307            -34                D                      NA
#> 308             18                N                      NA
#> 309             14                O                 8478413
#> 310             NA             <NA>                      NA
#> 311            -22                N                      NA
#> 312             NA             <NA>                      NA
#> 313             22                D                      NA
#> 314            -14                D                      NA
#> 315             -5                D                      NA
#> 316            -40                D                 8476292
#> 317             -9                O                      NA
#> 318             NA             <NA>                      NA
#> 319            -22                D                      NA
#> 320             NA             <NA>                      NA
#> 321            -22                O                      NA
#> 322              6                O                      NA
#> 323             NA             <NA>                      NA
#> 324             22                O                      NA
#> 325             19                O                      NA
#> 326              5                O                      NA
#> 327              9                O                      NA
#> 328             -6                O                      NA
#> 329              6                D                      NA
#> 330             NA             <NA>                      NA
#> 331             22                O                      NA
#> 332            -25                O                      NA
#> 333             31                D                      NA
#> 334             30                D                      NA
#> 335              2                O                      NA
#> 336              0                N                      NA
#> 337             39                O                      NA
#> 338             37                O                      NA
#> 339             NA             <NA>                      NA
#> 340             22                D                      NA
#> 341            -27                O                      NA
#> 342             -7                O                      NA
#> 343              6                O                      NA
#> 344             NA             <NA>                      NA
#> 345             22                O                      NA
#> 346             39                O                      NA
#> 347             39                O                      NA
#> 348             NA             <NA>                      NA
#> 349             NA             <NA>                      NA
#>     details.hitteePlayerId details.shotType details.shootingPlayerId
#> 1                       NA             <NA>                       NA
#> 2                       NA             <NA>                       NA
#> 3                  8475193             <NA>                       NA
#> 4                       NA            wrist                  8483495
#> 5                       NA             <NA>                       NA
#> 6                       NA            wrist                  8479407
#> 7                  8481559             <NA>                       NA
#> 8                       NA             slap                  8476462
#> 9                       NA             <NA>                  8479407
#> 10                      NA             <NA>                       NA
#> 11                      NA             <NA>                       NA
#> 12                      NA             <NA>                  8482175
#> 13                      NA            wrist                  8482175
#> 14                 8481524             <NA>                       NA
#> 15                      NA             <NA>                       NA
#> 16                      NA             <NA>                       NA
#> 17                      NA             slap                  8482671
#> 18                 8481528             <NA>                       NA
#> 19                      NA             <NA>                       NA
#> 20                      NA             <NA>                       NA
#> 21                 8479420             <NA>                       NA
#> 22                 8480839             <NA>                       NA
#> 23                 8482097             <NA>                       NA
#> 24                      NA             <NA>                       NA
#> 25                      NA            wrist                  8480002
#> 26                      NA            wrist                  8480002
#> 27                      NA            wrist                  8475722
#> 28                      NA             slap                  8480807
#> 29                      NA             <NA>                       NA
#> 30                      NA            wrist                  8478413
#> 31                 8477979             <NA>                       NA
#> 32                 8478399             <NA>                       NA
#> 33                      NA            wrist                  8479420
#> 34                      NA             <NA>                       NA
#> 35                      NA             <NA>                       NA
#> 36                      NA             <NA>                       NA
#> 37                 8480802             <NA>                       NA
#> 38                      NA             slap                  8475722
#> 39                      NA             <NA>                  8480035
#> 40                      NA             <NA>                       NA
#> 41                      NA             <NA>                       NA
#> 42                 8481032             <NA>                       NA
#> 43                 8478399             <NA>                       NA
#> 44                 8480192             <NA>                       NA
#> 45                 8478399             <NA>                       NA
#> 46                      NA             <NA>                       NA
#> 47                      NA             <NA>                       NA
#> 48                      NA           tip-in                  8477949
#> 49                      NA             <NA>                       NA
#> 50                      NA             <NA>                       NA
#> 51                 8483429             <NA>                       NA
#> 52                 8480002             <NA>                       NA
#> 53                 8480002             <NA>                       NA
#> 54                      NA             slap                  8478399
#> 55                 8476292             <NA>                       NA
#> 56                 8478413             <NA>                       NA
#> 57                      NA             snap                       NA
#> 58                      NA             <NA>                       NA
#> 59                      NA             <NA>                  8476462
#> 60                      NA             <NA>                  8475455
#> 61                      NA            wrist                  8478414
#> 62                 8480839             <NA>                       NA
#> 63                      NA      wrap-around                  8478414
#> 64                      NA             snap                  8475455
#> 65                      NA             <NA>                       NA
#> 66                      NA             <NA>                       NA
#> 67                      NA             <NA>                  8480002
#> 68                      NA             <NA>                       NA
#> 69                      NA             <NA>                       NA
#> 70                      NA             <NA>                       NA
#> 71                      NA             <NA>                  8481559
#> 72                      NA             <NA>                       NA
#> 73                      NA             <NA>                       NA
#> 74                      NA             <NA>                  8479407
#> 75                      NA             <NA>                  8479407
#> 76                      NA             <NA>                       NA
#> 77                 8481559             <NA>                       NA
#> 78                      NA            wrist                  8481559
#> 79                      NA             <NA>                  8480002
#> 80                      NA             <NA>                  8478414
#> 81                      NA             <NA>                  8480002
#> 82                 8477979             <NA>                       NA
#> 83                      NA             <NA>                       NA
#> 84                      NA             <NA>                  8482671
#> 85                      NA             <NA>                  8482175
#> 86                      NA            wrist                  8479407
#> 87                      NA             <NA>                       NA
#> 88                      NA             <NA>                       NA
#> 89                 8478413             <NA>                       NA
#> 90                      NA             <NA>                  8482175
#> 91                      NA             <NA>                       NA
#> 92                      NA             <NA>                       NA
#> 93                      NA            wrist                       NA
#> 94                      NA             <NA>                       NA
#> 95                      NA             <NA>                       NA
#> 96                      NA             <NA>                       NA
#> 97                      NA             slap                  8481528
#> 98                      NA             <NA>                       NA
#> 99                      NA             <NA>                       NA
#> 100                8482097             <NA>                       NA
#> 101                8482097             <NA>                       NA
#> 102                     NA            wrist                  8479359
#> 103                8478043             <NA>                       NA
#> 104                     NA             <NA>                  8480839
#> 105                     NA         backhand                  8477979
#> 106                8483429             <NA>                       NA
#> 107                     NA             <NA>                       NA
#> 108                     NA             <NA>                       NA
#> 109                     NA             <NA>                       NA
#> 110                     NA             <NA>                       NA
#> 111                     NA             <NA>                       NA
#> 112                     NA             <NA>                       NA
#> 113                     NA             <NA>                       NA
#> 114                     NA             snap                  8479407
#> 115                     NA             <NA>                       NA
#> 116                     NA             <NA>                  8478414
#> 117                     NA            wrist                  8482110
#> 118                     NA             <NA>                       NA
#> 119                8482671             <NA>                       NA
#> 120                     NA             <NA>                       NA
#> 121                     NA            wrist                  8482671
#> 122                     NA             <NA>                       NA
#> 123                     NA             <NA>                       NA
#> 124                     NA             <NA>                       NA
#> 125                     NA             <NA>                       NA
#> 126                     NA             <NA>                       NA
#> 127                     NA             snap                  8476462
#> 128                     NA             slap                  8475455
#> 129                8477949             <NA>                       NA
#> 130                     NA             <NA>                  8479420
#> 131                     NA           tip-in                  8482097
#> 132                8476292             <NA>                       NA
#> 133                     NA             <NA>                       NA
#> 134                     NA             <NA>                       NA
#> 135                     NA            wrist                  8476292
#> 136                     NA             <NA>                       NA
#> 137                     NA           tip-in                  8475722
#> 138                     NA         backhand                  8475722
#> 139                     NA             <NA>                       NA
#> 140                8480802             <NA>                       NA
#> 141                8480802             <NA>                       NA
#> 142                8479414             <NA>                       NA
#> 143                     NA            wrist                  8481032
#> 144                     NA            wrist                  8479414
#> 145                     NA            wrist                       NA
#> 146                     NA             <NA>                       NA
#> 147                     NA           tip-in                  8482110
#> 148                     NA             <NA>                  8483495
#> 149                8481524             <NA>                       NA
#> 150                8479407             <NA>                       NA
#> 151                     NA            wrist                  8477979
#> 152                     NA             <NA>                       NA
#> 153                     NA             <NA>                       NA
#> 154                     NA             <NA>                       NA
#> 155                8475455             <NA>                       NA
#> 156                     NA             <NA>                       NA
#> 157                     NA             <NA>                       NA
#> 158                     NA            wrist                  8481528
#> 159                     NA             <NA>                       NA
#> 160                     NA             <NA>                       NA
#> 161                     NA             <NA>                       NA
#> 162                     NA             <NA>                       NA
#> 163                     NA             <NA>                       NA
#> 164                     NA            wrist                  8481528
#> 165                     NA             <NA>                       NA
#> 166                     NA             <NA>                       NA
#> 167                     NA             snap                  8481528
#> 168                     NA            wrist                  8481528
#> 169                     NA             slap                  8475722
#> 170                     NA            wrist                  8481528
#> 171                     NA            wrist                  8484145
#> 172                     NA             <NA>                       NA
#> 173                     NA             <NA>                       NA
#> 174                     NA            wrist                  8478413
#> 175                8479420             <NA>                       NA
#> 176                8478413             <NA>                       NA
#> 177                     NA           tip-in                  8481032
#> 178                     NA             <NA>                       NA
#> 179                     NA             <NA>                       NA
#> 180                     NA           tip-in                  8479407
#> 181                     NA             slap                  8481559
#> 182                     NA            wrist                  8478413
#> 183                8482110             <NA>                       NA
#> 184                8477365             <NA>                       NA
#> 185                     NA             <NA>                       NA
#> 186                     NA             <NA>                       NA
#> 187                     NA           tip-in                  8480002
#> 188                8480002             <NA>                       NA
#> 189                8479359             <NA>                       NA
#> 190                8478399             <NA>                       NA
#> 191                     NA            wrist                  8481528
#> 192                     NA             <NA>                  8477949
#> 193                     NA             <NA>                  8482175
#> 194                8481032             <NA>                       NA
#> 195                     NA             <NA>                       NA
#> 196                     NA             <NA>                       NA
#> 197                     NA             <NA>                       NA
#> 198                     NA         backhand                  8477365
#> 199                     NA             <NA>                       NA
#> 200                     NA             <NA>                  8479420
#> 201                     NA            wrist                  8480807
#> 202                8477365             <NA>                       NA
#> 203                     NA             <NA>                  8480807
#> 204                     NA             <NA>                  8480807
#> 205                     NA             <NA>                       NA
#> 206                     NA             <NA>                       NA
#> 207                     NA             <NA>                       NA
#> 208                     NA             slap                  8475455
#> 209                     NA             slap                  8476462
#> 210                     NA             slap                  8476462
#> 211                     NA             <NA>                       NA
#> 212                     NA             <NA>                       NA
#> 213                     NA             <NA>                       NA
#> 214                     NA             <NA>                       NA
#> 215                     NA             <NA>                       NA
#> 216                     NA             <NA>                       NA
#> 217                     NA            wrist                  8479420
#> 218                     NA            wrist                  8482175
#> 219                     NA             <NA>                       NA
#> 220                     NA             <NA>                       NA
#> 221                     NA             <NA>                       NA
#> 222                     NA             slap                  8475722
#> 223                     NA             <NA>                  8481528
#> 224                     NA             <NA>                  8478414
#> 225                     NA             <NA>                       NA
#> 226                     NA             <NA>                       NA
#> 227                     NA             snap                  8482671
#> 228                8480192             <NA>                       NA
#> 229                     NA         backhand                  8478399
#> 230                8480807             <NA>                       NA
#> 231                     NA             <NA>                       NA
#> 232                     NA             <NA>                       NA
#> 233                     NA             <NA>                       NA
#> 234                     NA             <NA>                       NA
#> 235                     NA             <NA>                       NA
#> 236                     NA             <NA>                       NA
#> 237                     NA             <NA>                       NA
#> 238                     NA             <NA>                       NA
#> 239                     NA             <NA>                       NA
#> 240                     NA             <NA>                       NA
#> 241                     NA             <NA>                       NA
#> 242                8480002             <NA>                       NA
#> 243                     NA            wrist                  8479420
#> 244                     NA             <NA>                       NA
#> 245                     NA             <NA>                       NA
#> 246                8484145             <NA>                       NA
#> 247                8484145             <NA>                       NA
#> 248                     NA            wrist                  8482671
#> 249                     NA             <NA>                       NA
#> 250                     NA             <NA>                       NA
#> 251                8475287             <NA>                       NA
#> 252                8475455             <NA>                       NA
#> 253                     NA             <NA>                       NA
#> 254                     NA            wrist                  8479420
#> 255                8481032             <NA>                       NA
#> 256                     NA             <NA>                  8482097
#> 257                     NA             <NA>                       NA
#> 258                     NA             <NA>                  8483429
#> 259                     NA            wrist                  8480807
#> 260                     NA             <NA>                       NA
#> 261                     NA             <NA>                       NA
#> 262                     NA             <NA>                       NA
#> 263                     NA             <NA>                       NA
#> 264                     NA            wrist                  8482671
#> 265                     NA            wrist                  8484145
#> 266                     NA             <NA>                       NA
#> 267                     NA             <NA>                       NA
#> 268                     NA             <NA>                  8477949
#> 269                8480839             <NA>                       NA
#> 270                     NA             <NA>                  8477949
#> 271                     NA             <NA>                       NA
#> 272                     NA             <NA>                       NA
#> 273                     NA             snap                  8476462
#> 274                     NA             <NA>                       NA
#> 275                     NA             <NA>                       NA
#> 276                     NA            wrist                  8478413
#> 277                     NA             <NA>                       NA
#> 278                     NA             snap                  8477365
#> 279                     NA             <NA>                       NA
#> 280                     NA             <NA>                       NA
#> 281                     NA            wrist                  8480002
#> 282                     NA             <NA>                       NA
#> 283                     NA             <NA>                       NA
#> 284                     NA             <NA>                       NA
#> 285                     NA             <NA>                       NA
#> 286                     NA             <NA>                       NA
#> 287                     NA           tip-in                  8482175
#> 288                8479420             <NA>                       NA
#> 289                8477508             <NA>                       NA
#> 290                     NA             <NA>                       NA
#> 291                     NA             snap                  8481032
#> 292                8482671             <NA>                       NA
#> 293                8482671             <NA>                       NA
#> 294                     NA             snap                  8479420
#> 295                     NA            wrist                  8481524
#> 296                8482175             <NA>                       NA
#> 297                     NA            wrist                       NA
#> 298                     NA             <NA>                       NA
#> 299                8476292             <NA>                       NA
#> 300                     NA             <NA>                       NA
#> 301                     NA             <NA>                       NA
#> 302                8480839             <NA>                       NA
#> 303                     NA            wrist                  8484145
#> 304                     NA             <NA>                       NA
#> 305                     NA             <NA>                       NA
#> 306                     NA            wrist                  8477949
#> 307                     NA             <NA>                       NA
#> 308                     NA             <NA>                       NA
#> 309                8483495             <NA>                       NA
#> 310                     NA             <NA>                       NA
#> 311                     NA             <NA>                       NA
#> 312                     NA             <NA>                       NA
#> 313                     NA             <NA>                       NA
#> 314                     NA             <NA>                  8476462
#> 315                     NA             <NA>                  8481528
#> 316                8481528             <NA>                       NA
#> 317                     NA             slap                  8480839
#> 318                     NA             <NA>                       NA
#> 319                     NA             <NA>                       NA
#> 320                     NA             <NA>                       NA
#> 321                     NA             <NA>                       NA
#> 322                     NA            wrist                  8482671
#> 323                     NA             <NA>                       NA
#> 324                     NA             <NA>                       NA
#> 325                     NA            wrist                  8480839
#> 326                     NA         backhand                  8481528
#> 327                     NA            wrist                  8481528
#> 328                     NA         backhand                  8482175
#> 329                     NA             <NA>                  8482175
#> 330                     NA             <NA>                       NA
#> 331                     NA             <NA>                       NA
#> 332                     NA            wrist                  8478414
#> 333                     NA             <NA>                  8479420
#> 334                     NA             <NA>                  8479420
#> 335                     NA         backhand                       NA
#> 336                     NA             <NA>                       NA
#> 337                     NA             <NA>                       NA
#> 338                     NA            wrist                  8477365
#> 339                     NA             <NA>                       NA
#> 340                     NA             <NA>                       NA
#> 341                     NA             <NA>                       NA
#> 342                     NA           tip-in                  8476474
#> 343                     NA             snap                  8476292
#> 344                     NA             <NA>                       NA
#> 345                     NA             <NA>                       NA
#> 346                     NA             <NA>                       NA
#> 347                     NA             <NA>                       NA
#> 348                     NA             <NA>                       NA
#> 349                     NA             <NA>                       NA
#>     details.goalieInNetId details.awaySOG details.homeSOG details.playerId
#> 1                      NA              NA              NA               NA
#> 2                      NA              NA              NA               NA
#> 3                      NA              NA              NA               NA
#> 4                 8480045               1               0               NA
#> 5                      NA              NA              NA          8474593
#> 6                 8480045              NA              NA               NA
#> 7                      NA              NA              NA               NA
#> 8                 8480045               2               0               NA
#> 9                      NA              NA              NA               NA
#> 10                     NA              NA              NA               NA
#> 11                     NA              NA              NA               NA
#> 12                     NA              NA              NA               NA
#> 13                8474593               2               1               NA
#> 14                     NA              NA              NA               NA
#> 15                     NA              NA              NA               NA
#> 16                     NA              NA              NA               NA
#> 17                8474593              NA              NA               NA
#> 18                     NA              NA              NA               NA
#> 19                     NA              NA              NA               NA
#> 20                     NA              NA              NA               NA
#> 21                     NA              NA              NA               NA
#> 22                     NA              NA              NA               NA
#> 23                     NA              NA              NA               NA
#> 24                     NA              NA              NA          8481528
#> 25                8480045              NA              NA               NA
#> 26                8480045               3               1               NA
#> 27                8474593               3               2               NA
#> 28                8474593               3               3               NA
#> 29                     NA              NA              NA          8482110
#> 30                8474593              NA              NA               NA
#> 31                     NA              NA              NA               NA
#> 32                     NA              NA              NA               NA
#> 33                8474593              NA              NA               NA
#> 34                     NA              NA              NA          8474593
#> 35                     NA              NA              NA               NA
#> 36                     NA              NA              NA               NA
#> 37                     NA              NA              NA               NA
#> 38                8474593              NA              NA               NA
#> 39                     NA              NA              NA               NA
#> 40                     NA              NA              NA               NA
#> 41                     NA              NA              NA               NA
#> 42                     NA              NA              NA               NA
#> 43                     NA              NA              NA               NA
#> 44                     NA              NA              NA               NA
#> 45                     NA              NA              NA               NA
#> 46                     NA              NA              NA          8478399
#> 47                     NA              NA              NA          8474593
#> 48                8474593               3               4               NA
#> 49                     NA              NA              NA               NA
#> 50                     NA              NA              NA               NA
#> 51                     NA              NA              NA               NA
#> 52                     NA              NA              NA               NA
#> 53                     NA              NA              NA               NA
#> 54                8480045               4               4               NA
#> 55                     NA              NA              NA               NA
#> 56                     NA              NA              NA               NA
#> 57                8480045              NA              NA               NA
#> 58                     NA              NA              NA               NA
#> 59                     NA              NA              NA               NA
#> 60                     NA              NA              NA               NA
#> 61                8480045               5               4               NA
#> 62                     NA              NA              NA               NA
#> 63                8480045              NA              NA               NA
#> 64                8480045               6               4               NA
#> 65                     NA              NA              NA               NA
#> 66                     NA              NA              NA               NA
#> 67                     NA              NA              NA               NA
#> 68                     NA              NA              NA          8483495
#> 69                     NA              NA              NA               NA
#> 70                     NA              NA              NA               NA
#> 71                     NA              NA              NA               NA
#> 72                     NA              NA              NA               NA
#> 73                     NA              NA              NA               NA
#> 74                     NA              NA              NA               NA
#> 75                     NA              NA              NA               NA
#> 76                     NA              NA              NA          8477365
#> 77                     NA              NA              NA               NA
#> 78                8480045               7               4               NA
#> 79                     NA              NA              NA               NA
#> 80                     NA              NA              NA               NA
#> 81                     NA              NA              NA               NA
#> 82                     NA              NA              NA               NA
#> 83                     NA              NA              NA          8477949
#> 84                     NA              NA              NA               NA
#> 85                     NA              NA              NA               NA
#> 86                8480045               8               4               NA
#> 87                     NA              NA              NA               NA
#> 88                     NA              NA              NA               NA
#> 89                     NA              NA              NA               NA
#> 90                     NA              NA              NA               NA
#> 91                     NA              NA              NA               NA
#> 92                     NA              NA              NA               NA
#> 93                8480045              NA              NA               NA
#> 94                     NA              NA              NA               NA
#> 95                     NA              NA              NA               NA
#> 96                     NA              NA              NA               NA
#> 97                8474593               8               5               NA
#> 98                     NA              NA              NA               NA
#> 99                     NA              NA              NA               NA
#> 100                    NA              NA              NA               NA
#> 101                    NA              NA              NA               NA
#> 102               8474593               8               6               NA
#> 103                    NA              NA              NA               NA
#> 104                    NA              NA              NA               NA
#> 105               8474593               8               7               NA
#> 106                    NA              NA              NA               NA
#> 107                    NA              NA              NA               NA
#> 108                    NA              NA              NA               NA
#> 109                    NA              NA              NA               NA
#> 110                    NA              NA              NA               NA
#> 111                    NA              NA              NA               NA
#> 112                    NA              NA              NA               NA
#> 113                    NA              NA              NA               NA
#> 114               8480045               9               7               NA
#> 115                    NA              NA              NA          8478414
#> 116                    NA              NA              NA               NA
#> 117               8480045              NA              NA               NA
#> 118                    NA              NA              NA          8482110
#> 119                    NA              NA              NA               NA
#> 120                    NA              NA              NA          8482110
#> 121               8474593              NA              NA               NA
#> 122                    NA              NA              NA               NA
#> 123                    NA              NA              NA               NA
#> 124                    NA              NA              NA               NA
#> 125                    NA              NA              NA               NA
#> 126                    NA              NA              NA               NA
#> 127               8480045              10               7               NA
#> 128               8480045              NA              NA               NA
#> 129                    NA              NA              NA               NA
#> 130                    NA              NA              NA               NA
#> 131               8474593              NA              NA               NA
#> 132                    NA              NA              NA               NA
#> 133                    NA              NA              NA               NA
#> 134                    NA              NA              NA               NA
#> 135               8480045              11               7               NA
#> 136                    NA              NA              NA          8475287
#> 137               8474593              11               8               NA
#> 138               8474593              NA              NA               NA
#> 139                    NA              NA              NA          8480807
#> 140                    NA              NA              NA               NA
#> 141                    NA              NA              NA               NA
#> 142                    NA              NA              NA               NA
#> 143               8480045              12               8               NA
#> 144               8480045              13               8               NA
#> 145               8480045              NA              NA               NA
#> 146                    NA              NA              NA               NA
#> 147               8480045              NA              NA               NA
#> 148                    NA              NA              NA               NA
#> 149                    NA              NA              NA               NA
#> 150                    NA              NA              NA               NA
#> 151               8474593              13               9               NA
#> 152                    NA              NA              NA               NA
#> 153                    NA              NA              NA               NA
#> 154                    NA              NA              NA          8476474
#> 155                    NA              NA              NA               NA
#> 156                    NA              NA              NA          8475455
#> 157                    NA              NA              NA          8476474
#> 158               8474593              13              10               NA
#> 159                    NA              NA              NA               NA
#> 160                    NA              NA              NA               NA
#> 161                    NA              NA              NA               NA
#> 162                    NA              NA              NA               NA
#> 163                    NA              NA              NA               NA
#> 164               8474593              13              11               NA
#> 165                    NA              NA              NA               NA
#> 166                    NA              NA              NA               NA
#> 167               8474593              NA              NA               NA
#> 168               8474593              13              12               NA
#> 169               8474593              NA              NA               NA
#> 170               8474593              13              13               NA
#> 171               8474593              13              14               NA
#> 172                    NA              NA              NA               NA
#> 173                    NA              NA              NA               NA
#> 174               8474593              13              15               NA
#> 175                    NA              NA              NA               NA
#> 176                    NA              NA              NA               NA
#> 177               8480045              NA              NA               NA
#> 178                    NA              NA              NA          8478413
#> 179                    NA              NA              NA          8477949
#> 180               8480045              NA              NA               NA
#> 181               8480045              14              15               NA
#> 182               8474593              14              16               NA
#> 183                    NA              NA              NA               NA
#> 184                    NA              NA              NA               NA
#> 185                    NA              NA              NA               NA
#> 186                    NA              NA              NA               NA
#> 187               8480045              15              16               NA
#> 188                    NA              NA              NA               NA
#> 189                    NA              NA              NA               NA
#> 190                    NA              NA              NA               NA
#> 191               8474593              15              17               NA
#> 192                    NA              NA              NA               NA
#> 193                    NA              NA              NA               NA
#> 194                    NA              NA              NA               NA
#> 195                    NA              NA              NA               NA
#> 196                    NA              NA              NA               NA
#> 197                    NA              NA              NA          8483429
#> 198               8474593              NA              NA               NA
#> 199                    NA              NA              NA          8481559
#> 200                    NA              NA              NA               NA
#> 201               8474593              NA              NA               NA
#> 202                    NA              NA              NA               NA
#> 203                    NA              NA              NA               NA
#> 204                    NA              NA              NA               NA
#> 205                    NA              NA              NA               NA
#> 206                    NA              NA              NA               NA
#> 207                    NA              NA              NA          8481528
#> 208               8480045              NA              NA               NA
#> 209               8480045              NA              NA               NA
#> 210               8480045              16              17               NA
#> 211                    NA              NA              NA               NA
#> 212                    NA              NA              NA               NA
#> 213                    NA              NA              NA               NA
#> 214                    NA              NA              NA               NA
#> 215                    NA              NA              NA               NA
#> 216                    NA              NA              NA          8482110
#> 217               8474593              NA              NA               NA
#> 218               8474593              16              18               NA
#> 219                    NA              NA              NA               NA
#> 220                    NA              NA              NA               NA
#> 221                    NA              NA              NA          8481524
#> 222               8474593              NA              NA               NA
#> 223                    NA              NA              NA               NA
#> 224                    NA              NA              NA               NA
#> 225                    NA              NA              NA               NA
#> 226                    NA              NA              NA               NA
#> 227               8474593              NA              NA               NA
#> 228                    NA              NA              NA               NA
#> 229               8480045              NA              NA               NA
#> 230                    NA              NA              NA               NA
#> 231                    NA              NA              NA          8476292
#> 232                    NA              NA              NA               NA
#> 233                    NA              NA              NA               NA
#> 234                    NA              NA              NA               NA
#> 235                    NA              NA              NA               NA
#> 236                    NA              NA              NA               NA
#> 237                    NA              NA              NA               NA
#> 238                    NA              NA              NA               NA
#> 239                    NA              NA              NA          8476462
#> 240                    NA              NA              NA               NA
#> 241                    NA              NA              NA               NA
#> 242                    NA              NA              NA               NA
#> 243               8474593              16              19               NA
#> 244                    NA              NA              NA               NA
#> 245                    NA              NA              NA               NA
#> 246                    NA              NA              NA               NA
#> 247                    NA              NA              NA               NA
#> 248               8474593              NA              NA               NA
#> 249                    NA              NA              NA               NA
#> 250                    NA              NA              NA               NA
#> 251                    NA              NA              NA               NA
#> 252                    NA              NA              NA               NA
#> 253                    NA              NA              NA          8475455
#> 254               8474593              16              20               NA
#> 255                    NA              NA              NA               NA
#> 256                    NA              NA              NA               NA
#> 257                    NA              NA              NA          8480802
#> 258                    NA              NA              NA               NA
#> 259               8474593              16              21               NA
#> 260                    NA              NA              NA          8479420
#> 261                    NA              NA              NA          8475287
#> 262                    NA              NA              NA               NA
#> 263                    NA              NA              NA               NA
#> 264               8474593              16              22               NA
#> 265               8474593              NA              NA               NA
#> 266                    NA              NA              NA               NA
#> 267                    NA              NA              NA               NA
#> 268                    NA              NA              NA               NA
#> 269                    NA              NA              NA               NA
#> 270                    NA              NA              NA               NA
#> 271                    NA              NA              NA               NA
#> 272                    NA              NA              NA               NA
#> 273               8480045              17              22               NA
#> 274                    NA              NA              NA               NA
#> 275                    NA              NA              NA               NA
#> 276               8474593              NA              NA               NA
#> 277                    NA              NA              NA          8475455
#> 278               8474593              NA              NA               NA
#> 279                    NA              NA              NA               NA
#> 280                    NA              NA              NA               NA
#> 281               8480045              18              22               NA
#> 282                    NA              NA              NA               NA
#> 283                    NA              NA              NA               NA
#> 284                    NA              NA              NA          8480839
#> 285                    NA              NA              NA          8482110
#> 286                    NA              NA              NA          8475455
#> 287               8474593              NA              NA               NA
#> 288                    NA              NA              NA               NA
#> 289                    NA              NA              NA               NA
#> 290                    NA              NA              NA          8481524
#> 291               8480045              NA              NA               NA
#> 292                    NA              NA              NA               NA
#> 293                    NA              NA              NA               NA
#> 294               8474593              18              23               NA
#> 295               8474593              NA              NA               NA
#> 296                    NA              NA              NA               NA
#> 297               8474593              NA              NA               NA
#> 298                    NA              NA              NA               NA
#> 299                    NA              NA              NA               NA
#> 300                    NA              NA              NA               NA
#> 301                    NA              NA              NA               NA
#> 302                    NA              NA              NA               NA
#> 303               8474593              18              24               NA
#> 304                    NA              NA              NA               NA
#> 305                    NA              NA              NA               NA
#> 306               8474593              18              25               NA
#> 307                    NA              NA              NA          8480839
#> 308                    NA              NA              NA          8480807
#> 309                    NA              NA              NA               NA
#> 310                    NA              NA              NA               NA
#> 311                    NA              NA              NA               NA
#> 312                    NA              NA              NA               NA
#> 313                    NA              NA              NA               NA
#> 314                    NA              NA              NA               NA
#> 315                    NA              NA              NA               NA
#> 316                    NA              NA              NA               NA
#> 317               8474593              18              26               NA
#> 318                    NA              NA              NA               NA
#> 319                    NA              NA              NA               NA
#> 320                    NA              NA              NA               NA
#> 321                    NA              NA              NA               NA
#> 322               8474593              18              27               NA
#> 323                    NA              NA              NA               NA
#> 324                    NA              NA              NA               NA
#> 325               8474593              18              28               NA
#> 326               8474593              18              29               NA
#> 327               8474593              NA              NA               NA
#> 328               8474593              NA              NA               NA
#> 329                    NA              NA              NA               NA
#> 330                    NA              NA              NA               NA
#> 331                    NA              NA              NA               NA
#> 332               8480045              19              29               NA
#> 333                    NA              NA              NA               NA
#> 334                    NA              NA              NA               NA
#> 335                    NA              NA              NA               NA
#> 336                    NA              NA              NA               NA
#> 337                    NA              NA              NA          8484145
#> 338               8474593              19              30               NA
#> 339                    NA              NA              NA               NA
#> 340                    NA              NA              NA               NA
#> 341                    NA              NA              NA          8483495
#> 342               8480045              NA              NA               NA
#> 343               8480045              NA              NA               NA
#> 344                    NA              NA              NA               NA
#> 345                    NA              NA              NA               NA
#> 346                    NA              NA              NA          8482175
#> 347                    NA              NA              NA          8482175
#> 348                    NA              NA              NA               NA
#> 349                    NA              NA              NA               NA
#>                     details.reason details.blockingPlayerId details.typeCode
#> 1                             <NA>                       NA             <NA>
#> 2                             <NA>                       NA             <NA>
#> 3                             <NA>                       NA             <NA>
#> 4                             <NA>                       NA             <NA>
#> 5                             <NA>                       NA             <NA>
#> 6                            short                       NA             <NA>
#> 7                             <NA>                       NA             <NA>
#> 8                             <NA>                       NA             <NA>
#> 9                          blocked                  8481524             <NA>
#> 10                           icing                       NA             <NA>
#> 11                            <NA>                       NA             <NA>
#> 12                         blocked                  8475455             <NA>
#> 13                            <NA>                       NA             <NA>
#> 14                            <NA>                       NA             <NA>
#> 15        goalie-stopped-after-sog                       NA             <NA>
#> 16                            <NA>                       NA             <NA>
#> 17                      wide-right                       NA             <NA>
#> 18                            <NA>                       NA             <NA>
#> 19                            <NA>                       NA              MIN
#> 20                            <NA>                       NA             <NA>
#> 21                            <NA>                       NA             <NA>
#> 22                            <NA>                       NA             <NA>
#> 23                            <NA>                       NA             <NA>
#> 24                            <NA>                       NA             <NA>
#> 25                       wide-left                       NA             <NA>
#> 26                            <NA>                       NA             <NA>
#> 27                            <NA>                       NA             <NA>
#> 28                            <NA>                       NA             <NA>
#> 29                            <NA>                       NA             <NA>
#> 30                       wide-left                       NA             <NA>
#> 31                            <NA>                       NA             <NA>
#> 32                            <NA>                       NA             <NA>
#> 33                      wide-right                       NA             <NA>
#> 34                            <NA>                       NA             <NA>
#> 35                         offside                       NA             <NA>
#> 36                            <NA>                       NA             <NA>
#> 37                            <NA>                       NA             <NA>
#> 38                      wide-right                       NA             <NA>
#> 39                         blocked                  8475287             <NA>
#> 40                   puck-in-crowd                       NA             <NA>
#> 41                            <NA>                       NA             <NA>
#> 42                            <NA>                       NA             <NA>
#> 43                            <NA>                       NA             <NA>
#> 44                            <NA>                       NA             <NA>
#> 45                            <NA>                       NA             <NA>
#> 46                            <NA>                       NA             <NA>
#> 47                            <NA>                       NA             <NA>
#> 48                            <NA>                       NA             <NA>
#> 49                 puck-in-netting                       NA             <NA>
#> 50                            <NA>                       NA             <NA>
#> 51                            <NA>                       NA             <NA>
#> 52                            <NA>                       NA             <NA>
#> 53                            <NA>                       NA             <NA>
#> 54                            <NA>                       NA             <NA>
#> 55                            <NA>                       NA             <NA>
#> 56                            <NA>                       NA             <NA>
#> 57                            <NA>                       NA             <NA>
#> 58                            <NA>                       NA             <NA>
#> 59                         blocked                  8477949             <NA>
#> 60                         blocked                  8479420             <NA>
#> 61                            <NA>                       NA             <NA>
#> 62                            <NA>                       NA             <NA>
#> 63                      wide-right                       NA             <NA>
#> 64                            <NA>                       NA             <NA>
#> 65        goalie-stopped-after-sog                       NA             <NA>
#> 66                            <NA>                       NA             <NA>
#> 67                         blocked                  8479359             <NA>
#> 68                            <NA>                       NA             <NA>
#> 69                         offside                       NA             <NA>
#> 70                            <NA>                       NA             <NA>
#> 71                         blocked                  8480035             <NA>
#> 72                            <NA>                       NA              MIN
#> 73                            <NA>                       NA             <NA>
#> 74                         blocked                  8480807             <NA>
#> 75                         blocked                  8480807             <NA>
#> 76                            <NA>                       NA             <NA>
#> 77                            <NA>                       NA             <NA>
#> 78                            <NA>                       NA             <NA>
#> 79                         blocked                  8480035             <NA>
#> 80                         blocked                  8482671             <NA>
#> 81                         blocked                  8482671             <NA>
#> 82                            <NA>                       NA             <NA>
#> 83                            <NA>                       NA             <NA>
#> 84                         blocked                  8480192             <NA>
#> 85                         blocked                  8480192             <NA>
#> 86                            <NA>                       NA             <NA>
#> 87        goalie-stopped-after-sog                       NA             <NA>
#> 88                            <NA>                       NA             <NA>
#> 89                            <NA>                       NA             <NA>
#> 90                teammate-blocked                  8475722             <NA>
#> 91        goalie-stopped-after-sog                       NA             <NA>
#> 92                            <NA>                       NA             <NA>
#> 93                            <NA>                       NA             <NA>
#> 94                            <NA>                       NA             <NA>
#> 95                   puck-in-crowd                       NA             <NA>
#> 96                            <NA>                       NA             <NA>
#> 97                            <NA>                       NA             <NA>
#> 98        goalie-stopped-after-sog                       NA             <NA>
#> 99                            <NA>                       NA             <NA>
#> 100                           <NA>                       NA             <NA>
#> 101                           <NA>                       NA             <NA>
#> 102                           <NA>                       NA             <NA>
#> 103                           <NA>                       NA             <NA>
#> 104                        blocked                  8483495             <NA>
#> 105                           <NA>                       NA             <NA>
#> 106                           <NA>                       NA             <NA>
#> 107                           <NA>                       NA             <NA>
#> 108                           <NA>                       NA              MIN
#> 109                           <NA>                       NA             <NA>
#> 110                puck-in-benches                       NA             <NA>
#> 111                           <NA>                       NA             <NA>
#> 112                        offside                       NA             <NA>
#> 113                           <NA>                       NA             <NA>
#> 114                           <NA>                       NA             <NA>
#> 115                           <NA>                       NA             <NA>
#> 116                        blocked                  8480807             <NA>
#> 117                      wide-left                       NA             <NA>
#> 118                           <NA>                       NA             <NA>
#> 119                           <NA>                       NA             <NA>
#> 120                           <NA>                       NA             <NA>
#> 121                     wide-right                       NA             <NA>
#> 122                           <NA>                       NA             <NA>
#> 123                           <NA>                       NA             <NA>
#> 124                           <NA>                       NA             <NA>
#> 125                          icing                       NA             <NA>
#> 126                           <NA>                       NA             <NA>
#> 127                           <NA>                       NA             <NA>
#> 128             high-and-wide-left                       NA             <NA>
#> 129                           <NA>                       NA             <NA>
#> 130                        blocked                  8481559             <NA>
#> 131                      wide-left                       NA             <NA>
#> 132                           <NA>                       NA             <NA>
#> 133                    puck-frozen                       NA             <NA>
#> 134                           <NA>                       NA             <NA>
#> 135                           <NA>                       NA             <NA>
#> 136                           <NA>                       NA             <NA>
#> 137                           <NA>                       NA             <NA>
#> 138                      wide-left                       NA             <NA>
#> 139                           <NA>                       NA             <NA>
#> 140                           <NA>                       NA             <NA>
#> 141                           <NA>                       NA             <NA>
#> 142                           <NA>                       NA             <NA>
#> 143                           <NA>                       NA             <NA>
#> 144                           <NA>                       NA             <NA>
#> 145                           <NA>                       NA             <NA>
#> 146                           <NA>                       NA             <NA>
#> 147                      wide-left                       NA             <NA>
#> 148                        blocked                  8484145             <NA>
#> 149                           <NA>                       NA             <NA>
#> 150                           <NA>                       NA             <NA>
#> 151                           <NA>                       NA             <NA>
#> 152       goalie-stopped-after-sog                       NA             <NA>
#> 153                           <NA>                       NA             <NA>
#> 154                           <NA>                       NA             <NA>
#> 155                           <NA>                       NA             <NA>
#> 156                           <NA>                       NA             <NA>
#> 157                           <NA>                       NA             <NA>
#> 158                           <NA>                       NA             <NA>
#> 159                           <NA>                       NA              MIN
#> 160                     tv-timeout                       NA             <NA>
#> 161                           <NA>                       NA             <NA>
#> 162                     high-stick                       NA             <NA>
#> 163                           <NA>                       NA             <NA>
#> 164                           <NA>                       NA             <NA>
#> 165                puck-in-netting                       NA             <NA>
#> 166                           <NA>                       NA             <NA>
#> 167                 above-crossbar                       NA             <NA>
#> 168                           <NA>                       NA             <NA>
#> 169                 above-crossbar                       NA             <NA>
#> 170                           <NA>                       NA             <NA>
#> 171                           <NA>                       NA             <NA>
#> 172       goalie-stopped-after-sog                       NA             <NA>
#> 173                           <NA>                       NA             <NA>
#> 174                           <NA>                       NA             <NA>
#> 175                           <NA>                       NA             <NA>
#> 176                           <NA>                       NA             <NA>
#> 177                      wide-left                       NA             <NA>
#> 178                           <NA>                       NA             <NA>
#> 179                           <NA>                       NA             <NA>
#> 180                 above-crossbar                       NA             <NA>
#> 181                           <NA>                       NA             <NA>
#> 182                           <NA>                       NA             <NA>
#> 183                           <NA>                       NA             <NA>
#> 184                           <NA>                       NA             <NA>
#> 185                        offside                       NA             <NA>
#> 186                           <NA>                       NA             <NA>
#> 187                           <NA>                       NA             <NA>
#> 188                           <NA>                       NA             <NA>
#> 189                           <NA>                       NA             <NA>
#> 190                           <NA>                       NA             <NA>
#> 191                           <NA>                       NA             <NA>
#> 192                        blocked                  8478399             <NA>
#> 193                        blocked                  8480192             <NA>
#> 194                           <NA>                       NA             <NA>
#> 195                        offside                       NA             <NA>
#> 196                           <NA>                       NA             <NA>
#> 197                           <NA>                       NA             <NA>
#> 198                      wide-left                       NA             <NA>
#> 199                           <NA>                       NA             <NA>
#> 200                        blocked                  8483429             <NA>
#> 201                 above-crossbar                       NA             <NA>
#> 202                           <NA>                       NA             <NA>
#> 203                        blocked                  8479407             <NA>
#> 204               teammate-blocked                  8478413             <NA>
#> 205                        offside                       NA             <NA>
#> 206                           <NA>                       NA             <NA>
#> 207                           <NA>                       NA             <NA>
#> 208                      wide-left                       NA             <NA>
#> 209                 above-crossbar                       NA             <NA>
#> 210                           <NA>                       NA             <NA>
#> 211       goalie-stopped-after-sog                       NA             <NA>
#> 212                           <NA>                       NA             <NA>
#> 213                           <NA>                       NA             <NA>
#> 214                           <NA>                       NA              MIN
#> 215                           <NA>                       NA             <NA>
#> 216                           <NA>                       NA             <NA>
#> 217             high-and-wide-left                       NA             <NA>
#> 218                           <NA>                       NA             <NA>
#> 219                puck-in-netting                       NA             <NA>
#> 220                           <NA>                       NA             <NA>
#> 221                           <NA>                       NA             <NA>
#> 222                     wide-right                       NA             <NA>
#> 223                        blocked                  8478399             <NA>
#> 224                        blocked                  8480839             <NA>
#> 225                          icing                       NA             <NA>
#> 226                           <NA>                       NA             <NA>
#> 227                     wide-right                       NA             <NA>
#> 228                           <NA>                       NA             <NA>
#> 229                     wide-right                       NA             <NA>
#> 230                           <NA>                       NA             <NA>
#> 231                           <NA>                       NA             <NA>
#> 232                        offside                       NA             <NA>
#> 233                           <NA>                       NA             <NA>
#> 234                        offside                       NA             <NA>
#> 235                           <NA>                       NA             <NA>
#> 236                           <NA>                       NA             <NA>
#> 237                           <NA>                       NA             <NA>
#> 238                           <NA>                       NA             <NA>
#> 239                           <NA>                       NA             <NA>
#> 240                          icing                       NA             <NA>
#> 241                           <NA>                       NA             <NA>
#> 242                           <NA>                       NA             <NA>
#> 243                           <NA>                       NA             <NA>
#> 244       goalie-stopped-after-sog                       NA             <NA>
#> 245                           <NA>                       NA             <NA>
#> 246                           <NA>                       NA             <NA>
#> 247                           <NA>                       NA             <NA>
#> 248                      wide-left                       NA             <NA>
#> 249                          icing                       NA             <NA>
#> 250                           <NA>                       NA             <NA>
#> 251                           <NA>                       NA             <NA>
#> 252                           <NA>                       NA             <NA>
#> 253                           <NA>                       NA             <NA>
#> 254                           <NA>                       NA             <NA>
#> 255                           <NA>                       NA             <NA>
#> 256                        blocked                  8480002             <NA>
#> 257                           <NA>                       NA             <NA>
#> 258                        blocked                  8481524             <NA>
#> 259                           <NA>                       NA             <NA>
#> 260                           <NA>                       NA             <NA>
#> 261                           <NA>                       NA             <NA>
#> 262                           <NA>                       NA              MIN
#> 263                           <NA>                       NA             <NA>
#> 264                           <NA>                       NA             <NA>
#> 265                     wide-right                       NA             <NA>
#> 266 net-dislodged-defensive-skater                       NA             <NA>
#> 267                           <NA>                       NA             <NA>
#> 268                        blocked                  8483495             <NA>
#> 269                           <NA>                       NA             <NA>
#> 270                        blocked                  8483495             <NA>
#> 271                puck-in-netting                       NA             <NA>
#> 272                           <NA>                       NA             <NA>
#> 273                           <NA>                       NA             <NA>
#> 274       goalie-stopped-after-sog                       NA             <NA>
#> 275                           <NA>                       NA             <NA>
#> 276                     wide-right                       NA             <NA>
#> 277                           <NA>                       NA             <NA>
#> 278                      wide-left                       NA             <NA>
#> 279                  puck-in-crowd                       NA             <NA>
#> 280                           <NA>                       NA             <NA>
#> 281                           <NA>                       NA             <NA>
#> 282       goalie-stopped-after-sog                       NA             <NA>
#> 283                           <NA>                       NA             <NA>
#> 284                           <NA>                       NA             <NA>
#> 285                           <NA>                       NA             <NA>
#> 286                           <NA>                       NA             <NA>
#> 287            high-and-wide-right                       NA             <NA>
#> 288                           <NA>                       NA             <NA>
#> 289                           <NA>                       NA             <NA>
#> 290                           <NA>                       NA             <NA>
#> 291            high-and-wide-right                       NA             <NA>
#> 292                           <NA>                       NA             <NA>
#> 293                           <NA>                       NA             <NA>
#> 294                           <NA>                       NA             <NA>
#> 295                      wide-left                       NA             <NA>
#> 296                           <NA>                       NA             <NA>
#> 297                           <NA>                       NA             <NA>
#> 298                           <NA>                       NA             <NA>
#> 299                           <NA>                       NA             <NA>
#> 300                          icing                       NA             <NA>
#> 301                           <NA>                       NA             <NA>
#> 302                           <NA>                       NA             <NA>
#> 303                           <NA>                       NA             <NA>
#> 304                          icing                       NA             <NA>
#> 305                           <NA>                       NA             <NA>
#> 306                           <NA>                       NA             <NA>
#> 307                           <NA>                       NA             <NA>
#> 308                           <NA>                       NA             <NA>
#> 309                           <NA>                       NA             <NA>
#> 310                  puck-in-crowd                       NA             <NA>
#> 311                           <NA>                       NA             <NA>
#> 312                          icing                       NA             <NA>
#> 313                           <NA>                       NA             <NA>
#> 314                        blocked                  8481524             <NA>
#> 315                        blocked                  8476292             <NA>
#> 316                           <NA>                       NA             <NA>
#> 317                           <NA>                       NA             <NA>
#> 318       goalie-stopped-after-sog                       NA             <NA>
#> 319                           <NA>                       NA             <NA>
#> 320                        offside                       NA             <NA>
#> 321                           <NA>                       NA             <NA>
#> 322                           <NA>                       NA             <NA>
#> 323       goalie-stopped-after-sog                       NA             <NA>
#> 324                           <NA>                       NA             <NA>
#> 325                           <NA>                       NA             <NA>
#> 326                           <NA>                       NA             <NA>
#> 327                     wide-right                       NA             <NA>
#> 328                      wide-left                       NA             <NA>
#> 329                        blocked                  8478399             <NA>
#> 330                          icing                       NA             <NA>
#> 331                           <NA>                       NA             <NA>
#> 332                           <NA>                       NA             <NA>
#> 333                        blocked                  8481032             <NA>
#> 334                        blocked                  8481032             <NA>
#> 335                           <NA>                       NA             <NA>
#> 336                           <NA>                       NA             <NA>
#> 337                           <NA>                       NA             <NA>
#> 338                           <NA>                       NA             <NA>
#> 339       goalie-stopped-after-sog                       NA             <NA>
#> 340                           <NA>                       NA             <NA>
#> 341                           <NA>                       NA             <NA>
#> 342                      wide-left                       NA             <NA>
#> 343                 above-crossbar                       NA             <NA>
#> 344                          icing                       NA             <NA>
#> 345                           <NA>                       NA             <NA>
#> 346                           <NA>                       NA             <NA>
#> 347                           <NA>                       NA             <NA>
#> 348                           <NA>                       NA             <NA>
#> 349                           <NA>                       NA             <NA>
#>     details.descKey details.duration details.committedByPlayerId
#> 1              <NA>               NA                          NA
#> 2              <NA>               NA                          NA
#> 3              <NA>               NA                          NA
#> 4              <NA>               NA                          NA
#> 5              <NA>               NA                          NA
#> 6              <NA>               NA                          NA
#> 7              <NA>               NA                          NA
#> 8              <NA>               NA                          NA
#> 9              <NA>               NA                          NA
#> 10             <NA>               NA                          NA
#> 11             <NA>               NA                          NA
#> 12             <NA>               NA                          NA
#> 13             <NA>               NA                          NA
#> 14             <NA>               NA                          NA
#> 15             <NA>               NA                          NA
#> 16             <NA>               NA                          NA
#> 17             <NA>               NA                          NA
#> 18             <NA>               NA                          NA
#> 19         slashing                2                     8475287
#> 20             <NA>               NA                          NA
#> 21             <NA>               NA                          NA
#> 22             <NA>               NA                          NA
#> 23             <NA>               NA                          NA
#> 24             <NA>               NA                          NA
#> 25             <NA>               NA                          NA
#> 26             <NA>               NA                          NA
#> 27             <NA>               NA                          NA
#> 28             <NA>               NA                          NA
#> 29             <NA>               NA                          NA
#> 30             <NA>               NA                          NA
#> 31             <NA>               NA                          NA
#> 32             <NA>               NA                          NA
#> 33             <NA>               NA                          NA
#> 34             <NA>               NA                          NA
#> 35             <NA>               NA                          NA
#> 36             <NA>               NA                          NA
#> 37             <NA>               NA                          NA
#> 38             <NA>               NA                          NA
#> 39             <NA>               NA                          NA
#> 40             <NA>               NA                          NA
#> 41             <NA>               NA                          NA
#> 42             <NA>               NA                          NA
#> 43             <NA>               NA                          NA
#> 44             <NA>               NA                          NA
#> 45             <NA>               NA                          NA
#> 46             <NA>               NA                          NA
#> 47             <NA>               NA                          NA
#> 48             <NA>               NA                          NA
#> 49             <NA>               NA                          NA
#> 50             <NA>               NA                          NA
#> 51             <NA>               NA                          NA
#> 52             <NA>               NA                          NA
#> 53             <NA>               NA                          NA
#> 54             <NA>               NA                          NA
#> 55             <NA>               NA                          NA
#> 56             <NA>               NA                          NA
#> 57             <NA>               NA                          NA
#> 58             <NA>               NA                          NA
#> 59             <NA>               NA                          NA
#> 60             <NA>               NA                          NA
#> 61             <NA>               NA                          NA
#> 62             <NA>               NA                          NA
#> 63             <NA>               NA                          NA
#> 64             <NA>               NA                          NA
#> 65             <NA>               NA                          NA
#> 66             <NA>               NA                          NA
#> 67             <NA>               NA                          NA
#> 68             <NA>               NA                          NA
#> 69             <NA>               NA                          NA
#> 70             <NA>               NA                          NA
#> 71             <NA>               NA                          NA
#> 72     interference                2                     8480839
#> 73             <NA>               NA                          NA
#> 74             <NA>               NA                          NA
#> 75             <NA>               NA                          NA
#> 76             <NA>               NA                          NA
#> 77             <NA>               NA                          NA
#> 78             <NA>               NA                          NA
#> 79             <NA>               NA                          NA
#> 80             <NA>               NA                          NA
#> 81             <NA>               NA                          NA
#> 82             <NA>               NA                          NA
#> 83             <NA>               NA                          NA
#> 84             <NA>               NA                          NA
#> 85             <NA>               NA                          NA
#> 86             <NA>               NA                          NA
#> 87             <NA>               NA                          NA
#> 88             <NA>               NA                          NA
#> 89             <NA>               NA                          NA
#> 90             <NA>               NA                          NA
#> 91             <NA>               NA                          NA
#> 92             <NA>               NA                          NA
#> 93             <NA>               NA                          NA
#> 94             <NA>               NA                          NA
#> 95             <NA>               NA                          NA
#> 96             <NA>               NA                          NA
#> 97             <NA>               NA                          NA
#> 98             <NA>               NA                          NA
#> 99             <NA>               NA                          NA
#> 100            <NA>               NA                          NA
#> 101            <NA>               NA                          NA
#> 102            <NA>               NA                          NA
#> 103            <NA>               NA                          NA
#> 104            <NA>               NA                          NA
#> 105            <NA>               NA                          NA
#> 106            <NA>               NA                          NA
#> 107            <NA>               NA                          NA
#> 108        slashing                2                     8477979
#> 109            <NA>               NA                          NA
#> 110            <NA>               NA                          NA
#> 111            <NA>               NA                          NA
#> 112            <NA>               NA                          NA
#> 113            <NA>               NA                          NA
#> 114            <NA>               NA                          NA
#> 115            <NA>               NA                          NA
#> 116            <NA>               NA                          NA
#> 117            <NA>               NA                          NA
#> 118            <NA>               NA                          NA
#> 119            <NA>               NA                          NA
#> 120            <NA>               NA                          NA
#> 121            <NA>               NA                          NA
#> 122            <NA>               NA                          NA
#> 123            <NA>               NA                          NA
#> 124            <NA>               NA                          NA
#> 125            <NA>               NA                          NA
#> 126            <NA>               NA                          NA
#> 127            <NA>               NA                          NA
#> 128            <NA>               NA                          NA
#> 129            <NA>               NA                          NA
#> 130            <NA>               NA                          NA
#> 131            <NA>               NA                          NA
#> 132            <NA>               NA                          NA
#> 133            <NA>               NA                          NA
#> 134            <NA>               NA                          NA
#> 135            <NA>               NA                          NA
#> 136            <NA>               NA                          NA
#> 137            <NA>               NA                          NA
#> 138            <NA>               NA                          NA
#> 139            <NA>               NA                          NA
#> 140            <NA>               NA                          NA
#> 141            <NA>               NA                          NA
#> 142            <NA>               NA                          NA
#> 143            <NA>               NA                          NA
#> 144            <NA>               NA                          NA
#> 145            <NA>               NA                          NA
#> 146            <NA>               NA                          NA
#> 147            <NA>               NA                          NA
#> 148            <NA>               NA                          NA
#> 149            <NA>               NA                          NA
#> 150            <NA>               NA                          NA
#> 151            <NA>               NA                          NA
#> 152            <NA>               NA                          NA
#> 153            <NA>               NA                          NA
#> 154            <NA>               NA                          NA
#> 155            <NA>               NA                          NA
#> 156            <NA>               NA                          NA
#> 157            <NA>               NA                          NA
#> 158            <NA>               NA                          NA
#> 159         holding                2                     8477508
#> 160            <NA>               NA                          NA
#> 161            <NA>               NA                          NA
#> 162            <NA>               NA                          NA
#> 163            <NA>               NA                          NA
#> 164            <NA>               NA                          NA
#> 165            <NA>               NA                          NA
#> 166            <NA>               NA                          NA
#> 167            <NA>               NA                          NA
#> 168            <NA>               NA                          NA
#> 169            <NA>               NA                          NA
#> 170            <NA>               NA                          NA
#> 171            <NA>               NA                          NA
#> 172            <NA>               NA                          NA
#> 173            <NA>               NA                          NA
#> 174            <NA>               NA                          NA
#> 175            <NA>               NA                          NA
#> 176            <NA>               NA                          NA
#> 177            <NA>               NA                          NA
#> 178            <NA>               NA                          NA
#> 179            <NA>               NA                          NA
#> 180            <NA>               NA                          NA
#> 181            <NA>               NA                          NA
#> 182            <NA>               NA                          NA
#> 183            <NA>               NA                          NA
#> 184            <NA>               NA                          NA
#> 185            <NA>               NA                          NA
#> 186            <NA>               NA                          NA
#> 187            <NA>               NA                          NA
#> 188            <NA>               NA                          NA
#> 189            <NA>               NA                          NA
#> 190            <NA>               NA                          NA
#> 191            <NA>               NA                          NA
#> 192            <NA>               NA                          NA
#> 193            <NA>               NA                          NA
#> 194            <NA>               NA                          NA
#> 195            <NA>               NA                          NA
#> 196            <NA>               NA                          NA
#> 197            <NA>               NA                          NA
#> 198            <NA>               NA                          NA
#> 199            <NA>               NA                          NA
#> 200            <NA>               NA                          NA
#> 201            <NA>               NA                          NA
#> 202            <NA>               NA                          NA
#> 203            <NA>               NA                          NA
#> 204            <NA>               NA                          NA
#> 205            <NA>               NA                          NA
#> 206            <NA>               NA                          NA
#> 207            <NA>               NA                          NA
#> 208            <NA>               NA                          NA
#> 209            <NA>               NA                          NA
#> 210            <NA>               NA                          NA
#> 211            <NA>               NA                          NA
#> 212            <NA>               NA                          NA
#> 213            <NA>               NA                          NA
#> 214         holding                2                     8483495
#> 215            <NA>               NA                          NA
#> 216            <NA>               NA                          NA
#> 217            <NA>               NA                          NA
#> 218            <NA>               NA                          NA
#> 219            <NA>               NA                          NA
#> 220            <NA>               NA                          NA
#> 221            <NA>               NA                          NA
#> 222            <NA>               NA                          NA
#> 223            <NA>               NA                          NA
#> 224            <NA>               NA                          NA
#> 225            <NA>               NA                          NA
#> 226            <NA>               NA                          NA
#> 227            <NA>               NA                          NA
#> 228            <NA>               NA                          NA
#> 229            <NA>               NA                          NA
#> 230            <NA>               NA                          NA
#> 231            <NA>               NA                          NA
#> 232            <NA>               NA                          NA
#> 233            <NA>               NA                          NA
#> 234            <NA>               NA                          NA
#> 235            <NA>               NA                          NA
#> 236            <NA>               NA                          NA
#> 237            <NA>               NA                          NA
#> 238            <NA>               NA                          NA
#> 239            <NA>               NA                          NA
#> 240            <NA>               NA                          NA
#> 241            <NA>               NA                          NA
#> 242            <NA>               NA                          NA
#> 243            <NA>               NA                          NA
#> 244            <NA>               NA                          NA
#> 245            <NA>               NA                          NA
#> 246            <NA>               NA                          NA
#> 247            <NA>               NA                          NA
#> 248            <NA>               NA                          NA
#> 249            <NA>               NA                          NA
#> 250            <NA>               NA                          NA
#> 251            <NA>               NA                          NA
#> 252            <NA>               NA                          NA
#> 253            <NA>               NA                          NA
#> 254            <NA>               NA                          NA
#> 255            <NA>               NA                          NA
#> 256            <NA>               NA                          NA
#> 257            <NA>               NA                          NA
#> 258            <NA>               NA                          NA
#> 259            <NA>               NA                          NA
#> 260            <NA>               NA                          NA
#> 261            <NA>               NA                          NA
#> 262         holding                2                     8479414
#> 263            <NA>               NA                          NA
#> 264            <NA>               NA                          NA
#> 265            <NA>               NA                          NA
#> 266            <NA>               NA                          NA
#> 267            <NA>               NA                          NA
#> 268            <NA>               NA                          NA
#> 269            <NA>               NA                          NA
#> 270            <NA>               NA                          NA
#> 271            <NA>               NA                          NA
#> 272            <NA>               NA                          NA
#> 273            <NA>               NA                          NA
#> 274            <NA>               NA                          NA
#> 275            <NA>               NA                          NA
#> 276            <NA>               NA                          NA
#> 277            <NA>               NA                          NA
#> 278            <NA>               NA                          NA
#> 279            <NA>               NA                          NA
#> 280            <NA>               NA                          NA
#> 281            <NA>               NA                          NA
#> 282            <NA>               NA                          NA
#> 283            <NA>               NA                          NA
#> 284            <NA>               NA                          NA
#> 285            <NA>               NA                          NA
#> 286            <NA>               NA                          NA
#> 287            <NA>               NA                          NA
#> 288            <NA>               NA                          NA
#> 289            <NA>               NA                          NA
#> 290            <NA>               NA                          NA
#> 291            <NA>               NA                          NA
#> 292            <NA>               NA                          NA
#> 293            <NA>               NA                          NA
#> 294            <NA>               NA                          NA
#> 295            <NA>               NA                          NA
#> 296            <NA>               NA                          NA
#> 297            <NA>               NA                          NA
#> 298            <NA>               NA                          NA
#> 299            <NA>               NA                          NA
#> 300            <NA>               NA                          NA
#> 301            <NA>               NA                          NA
#> 302            <NA>               NA                          NA
#> 303            <NA>               NA                          NA
#> 304            <NA>               NA                          NA
#> 305            <NA>               NA                          NA
#> 306            <NA>               NA                          NA
#> 307            <NA>               NA                          NA
#> 308            <NA>               NA                          NA
#> 309            <NA>               NA                          NA
#> 310            <NA>               NA                          NA
#> 311            <NA>               NA                          NA
#> 312            <NA>               NA                          NA
#> 313            <NA>               NA                          NA
#> 314            <NA>               NA                          NA
#> 315            <NA>               NA                          NA
#> 316            <NA>               NA                          NA
#> 317            <NA>               NA                          NA
#> 318            <NA>               NA                          NA
#> 319            <NA>               NA                          NA
#> 320            <NA>               NA                          NA
#> 321            <NA>               NA                          NA
#> 322            <NA>               NA                          NA
#> 323            <NA>               NA                          NA
#> 324            <NA>               NA                          NA
#> 325            <NA>               NA                          NA
#> 326            <NA>               NA                          NA
#> 327            <NA>               NA                          NA
#> 328            <NA>               NA                          NA
#> 329            <NA>               NA                          NA
#> 330            <NA>               NA                          NA
#> 331            <NA>               NA                          NA
#> 332            <NA>               NA                          NA
#> 333            <NA>               NA                          NA
#> 334            <NA>               NA                          NA
#> 335            <NA>               NA                          NA
#> 336            <NA>               NA                          NA
#> 337            <NA>               NA                          NA
#> 338            <NA>               NA                          NA
#> 339            <NA>               NA                          NA
#> 340            <NA>               NA                          NA
#> 341            <NA>               NA                          NA
#> 342            <NA>               NA                          NA
#> 343            <NA>               NA                          NA
#> 344            <NA>               NA                          NA
#> 345            <NA>               NA                          NA
#> 346            <NA>               NA                          NA
#> 347            <NA>               NA                          NA
#> 348            <NA>               NA                          NA
#> 349            <NA>               NA                          NA
#>     details.drawnByPlayerId details.secondaryReason details.scoringPlayerId
#> 1                        NA                    <NA>                      NA
#> 2                        NA                    <NA>                      NA
#> 3                        NA                    <NA>                      NA
#> 4                        NA                    <NA>                      NA
#> 5                        NA                    <NA>                      NA
#> 6                        NA                    <NA>                      NA
#> 7                        NA                    <NA>                      NA
#> 8                        NA                    <NA>                      NA
#> 9                        NA                    <NA>                      NA
#> 10                       NA                    <NA>                      NA
#> 11                       NA                    <NA>                      NA
#> 12                       NA                    <NA>                      NA
#> 13                       NA                    <NA>                      NA
#> 14                       NA                    <NA>                      NA
#> 15                       NA                    <NA>                      NA
#> 16                       NA                    <NA>                      NA
#> 17                       NA                    <NA>                      NA
#> 18                       NA                    <NA>                      NA
#> 19                  8479420                    <NA>                      NA
#> 20                       NA                    <NA>                      NA
#> 21                       NA                    <NA>                      NA
#> 22                       NA                    <NA>                      NA
#> 23                       NA                    <NA>                      NA
#> 24                       NA                    <NA>                      NA
#> 25                       NA                    <NA>                      NA
#> 26                       NA                    <NA>                      NA
#> 27                       NA                    <NA>                      NA
#> 28                       NA                    <NA>                      NA
#> 29                       NA                    <NA>                      NA
#> 30                       NA                    <NA>                      NA
#> 31                       NA                    <NA>                      NA
#> 32                       NA                    <NA>                      NA
#> 33                       NA                    <NA>                      NA
#> 34                       NA                    <NA>                      NA
#> 35                       NA                    <NA>                      NA
#> 36                       NA                    <NA>                      NA
#> 37                       NA                    <NA>                      NA
#> 38                       NA                    <NA>                      NA
#> 39                       NA                    <NA>                      NA
#> 40                       NA              tv-timeout                      NA
#> 41                       NA                    <NA>                      NA
#> 42                       NA                    <NA>                      NA
#> 43                       NA                    <NA>                      NA
#> 44                       NA                    <NA>                      NA
#> 45                       NA                    <NA>                      NA
#> 46                       NA                    <NA>                      NA
#> 47                       NA                    <NA>                      NA
#> 48                       NA                    <NA>                      NA
#> 49                       NA                    <NA>                      NA
#> 50                       NA                    <NA>                      NA
#> 51                       NA                    <NA>                      NA
#> 52                       NA                    <NA>                      NA
#> 53                       NA                    <NA>                      NA
#> 54                       NA                    <NA>                      NA
#> 55                       NA                    <NA>                      NA
#> 56                       NA                    <NA>                      NA
#> 57                       NA                    <NA>                 8476474
#> 58                       NA                    <NA>                      NA
#> 59                       NA                    <NA>                      NA
#> 60                       NA                    <NA>                      NA
#> 61                       NA                    <NA>                      NA
#> 62                       NA                    <NA>                      NA
#> 63                       NA                    <NA>                      NA
#> 64                       NA                    <NA>                      NA
#> 65                       NA                    <NA>                      NA
#> 66                       NA                    <NA>                      NA
#> 67                       NA                    <NA>                      NA
#> 68                       NA                    <NA>                      NA
#> 69                       NA              tv-timeout                      NA
#> 70                       NA                    <NA>                      NA
#> 71                       NA                    <NA>                      NA
#> 72                  8479407                    <NA>                      NA
#> 73                       NA                    <NA>                      NA
#> 74                       NA                    <NA>                      NA
#> 75                       NA                    <NA>                      NA
#> 76                       NA                    <NA>                      NA
#> 77                       NA                    <NA>                      NA
#> 78                       NA                    <NA>                      NA
#> 79                       NA                    <NA>                      NA
#> 80                       NA                    <NA>                      NA
#> 81                       NA                    <NA>                      NA
#> 82                       NA                    <NA>                      NA
#> 83                       NA                    <NA>                      NA
#> 84                       NA                    <NA>                      NA
#> 85                       NA                    <NA>                      NA
#> 86                       NA                    <NA>                      NA
#> 87                       NA              tv-timeout                      NA
#> 88                       NA                    <NA>                      NA
#> 89                       NA                    <NA>                      NA
#> 90                       NA                    <NA>                      NA
#> 91                       NA                    <NA>                      NA
#> 92                       NA                    <NA>                      NA
#> 93                       NA                    <NA>                 8480192
#> 94                       NA                    <NA>                      NA
#> 95                       NA                    <NA>                      NA
#> 96                       NA                    <NA>                      NA
#> 97                       NA                    <NA>                      NA
#> 98                       NA                    <NA>                      NA
#> 99                       NA                    <NA>                      NA
#> 100                      NA                    <NA>                      NA
#> 101                      NA                    <NA>                      NA
#> 102                      NA                    <NA>                      NA
#> 103                      NA                    <NA>                      NA
#> 104                      NA                    <NA>                      NA
#> 105                      NA                    <NA>                      NA
#> 106                      NA                    <NA>                      NA
#> 107                      NA                    <NA>                      NA
#> 108                 8481559                    <NA>                      NA
#> 109                      NA                    <NA>                      NA
#> 110                      NA                    <NA>                      NA
#> 111                      NA                    <NA>                      NA
#> 112                      NA                    <NA>                      NA
#> 113                      NA                    <NA>                      NA
#> 114                      NA                    <NA>                      NA
#> 115                      NA                    <NA>                      NA
#> 116                      NA                    <NA>                      NA
#> 117                      NA                    <NA>                      NA
#> 118                      NA                    <NA>                      NA
#> 119                      NA                    <NA>                      NA
#> 120                      NA                    <NA>                      NA
#> 121                      NA                    <NA>                      NA
#> 122                      NA                    <NA>                      NA
#> 123                      NA                    <NA>                      NA
#> 124                      NA                    <NA>                      NA
#> 125                      NA                    <NA>                      NA
#> 126                      NA                    <NA>                      NA
#> 127                      NA                    <NA>                      NA
#> 128                      NA                    <NA>                      NA
#> 129                      NA                    <NA>                      NA
#> 130                      NA                    <NA>                      NA
#> 131                      NA                    <NA>                      NA
#> 132                      NA                    <NA>                      NA
#> 133                      NA                    <NA>                      NA
#> 134                      NA                    <NA>                      NA
#> 135                      NA                    <NA>                      NA
#> 136                      NA                    <NA>                      NA
#> 137                      NA                    <NA>                      NA
#> 138                      NA                    <NA>                      NA
#> 139                      NA                    <NA>                      NA
#> 140                      NA                    <NA>                      NA
#> 141                      NA                    <NA>                      NA
#> 142                      NA                    <NA>                      NA
#> 143                      NA                    <NA>                      NA
#> 144                      NA                    <NA>                      NA
#> 145                      NA                    <NA>                 8480002
#> 146                      NA                    <NA>                      NA
#> 147                      NA                    <NA>                      NA
#> 148                      NA                    <NA>                      NA
#> 149                      NA                    <NA>                      NA
#> 150                      NA                    <NA>                      NA
#> 151                      NA                    <NA>                      NA
#> 152                      NA        player-equipment                      NA
#> 153                      NA                    <NA>                      NA
#> 154                      NA                    <NA>                      NA
#> 155                      NA                    <NA>                      NA
#> 156                      NA                    <NA>                      NA
#> 157                      NA                    <NA>                      NA
#> 158                      NA                    <NA>                      NA
#> 159                 8482175                    <NA>                      NA
#> 160                      NA              tv-timeout                      NA
#> 161                      NA                    <NA>                      NA
#> 162                      NA                    <NA>                      NA
#> 163                      NA                    <NA>                      NA
#> 164                      NA                    <NA>                      NA
#> 165                      NA                    <NA>                      NA
#> 166                      NA                    <NA>                      NA
#> 167                      NA                    <NA>                      NA
#> 168                      NA                    <NA>                      NA
#> 169                      NA                    <NA>                      NA
#> 170                      NA                    <NA>                      NA
#> 171                      NA                    <NA>                      NA
#> 172                      NA                    <NA>                      NA
#> 173                      NA                    <NA>                      NA
#> 174                      NA                    <NA>                      NA
#> 175                      NA                    <NA>                      NA
#> 176                      NA                    <NA>                      NA
#> 177                      NA                    <NA>                      NA
#> 178                      NA                    <NA>                      NA
#> 179                      NA                    <NA>                      NA
#> 180                      NA                    <NA>                      NA
#> 181                      NA                    <NA>                      NA
#> 182                      NA                    <NA>                      NA
#> 183                      NA                    <NA>                      NA
#> 184                      NA                    <NA>                      NA
#> 185                      NA              tv-timeout                      NA
#> 186                      NA                    <NA>                      NA
#> 187                      NA                    <NA>                      NA
#> 188                      NA                    <NA>                      NA
#> 189                      NA                    <NA>                      NA
#> 190                      NA                    <NA>                      NA
#> 191                      NA                    <NA>                      NA
#> 192                      NA                    <NA>                      NA
#> 193                      NA                    <NA>                      NA
#> 194                      NA                    <NA>                      NA
#> 195                      NA                    <NA>                      NA
#> 196                      NA                    <NA>                      NA
#> 197                      NA                    <NA>                      NA
#> 198                      NA                    <NA>                      NA
#> 199                      NA                    <NA>                      NA
#> 200                      NA                    <NA>                      NA
#> 201                      NA                    <NA>                      NA
#> 202                      NA                    <NA>                      NA
#> 203                      NA                    <NA>                      NA
#> 204                      NA                    <NA>                      NA
#> 205                      NA                    <NA>                      NA
#> 206                      NA                    <NA>                      NA
#> 207                      NA                    <NA>                      NA
#> 208                      NA                    <NA>                      NA
#> 209                      NA                    <NA>                      NA
#> 210                      NA                    <NA>                      NA
#> 211                      NA              tv-timeout                      NA
#> 212                      NA                    <NA>                      NA
#> 213                      NA                    <NA>                      NA
#> 214                 8478413                    <NA>                      NA
#> 215                      NA                    <NA>                      NA
#> 216                      NA                    <NA>                      NA
#> 217                      NA                    <NA>                      NA
#> 218                      NA                    <NA>                      NA
#> 219                      NA                    <NA>                      NA
#> 220                      NA                    <NA>                      NA
#> 221                      NA                    <NA>                      NA
#> 222                      NA                    <NA>                      NA
#> 223                      NA                    <NA>                      NA
#> 224                      NA                    <NA>                      NA
#> 225                      NA                    <NA>                      NA
#> 226                      NA                    <NA>                      NA
#> 227                      NA                    <NA>                      NA
#> 228                      NA                    <NA>                      NA
#> 229                      NA                    <NA>                      NA
#> 230                      NA                    <NA>                      NA
#> 231                      NA                    <NA>                      NA
#> 232                      NA                    <NA>                      NA
#> 233                      NA                    <NA>                      NA
#> 234                      NA                    <NA>                      NA
#> 235                      NA                    <NA>                      NA
#> 236                      NA                    <NA>                      NA
#> 237                      NA                    <NA>                      NA
#> 238                      NA                    <NA>                      NA
#> 239                      NA                    <NA>                      NA
#> 240                      NA                    <NA>                      NA
#> 241                      NA                    <NA>                      NA
#> 242                      NA                    <NA>                      NA
#> 243                      NA                    <NA>                      NA
#> 244                      NA                    <NA>                      NA
#> 245                      NA                    <NA>                      NA
#> 246                      NA                    <NA>                      NA
#> 247                      NA                    <NA>                      NA
#> 248                      NA                    <NA>                      NA
#> 249                      NA                    <NA>                      NA
#> 250                      NA                    <NA>                      NA
#> 251                      NA                    <NA>                      NA
#> 252                      NA                    <NA>                      NA
#> 253                      NA                    <NA>                      NA
#> 254                      NA                    <NA>                      NA
#> 255                      NA                    <NA>                      NA
#> 256                      NA                    <NA>                      NA
#> 257                      NA                    <NA>                      NA
#> 258                      NA                    <NA>                      NA
#> 259                      NA                    <NA>                      NA
#> 260                      NA                    <NA>                      NA
#> 261                      NA                    <NA>                      NA
#> 262                 8482097                    <NA>                      NA
#> 263                      NA                    <NA>                      NA
#> 264                      NA                    <NA>                      NA
#> 265                      NA                    <NA>                      NA
#> 266                      NA                    <NA>                      NA
#> 267                      NA                    <NA>                      NA
#> 268                      NA                    <NA>                      NA
#> 269                      NA                    <NA>                      NA
#> 270                      NA                    <NA>                      NA
#> 271                      NA                    <NA>                      NA
#> 272                      NA                    <NA>                      NA
#> 273                      NA                    <NA>                      NA
#> 274                      NA              tv-timeout                      NA
#> 275                      NA                    <NA>                      NA
#> 276                      NA                    <NA>                      NA
#> 277                      NA                    <NA>                      NA
#> 278                      NA                    <NA>                      NA
#> 279                      NA                    <NA>                      NA
#> 280                      NA                    <NA>                      NA
#> 281                      NA                    <NA>                      NA
#> 282                      NA                    <NA>                      NA
#> 283                      NA                    <NA>                      NA
#> 284                      NA                    <NA>                      NA
#> 285                      NA                    <NA>                      NA
#> 286                      NA                    <NA>                      NA
#> 287                      NA                    <NA>                      NA
#> 288                      NA                    <NA>                      NA
#> 289                      NA                    <NA>                      NA
#> 290                      NA                    <NA>                      NA
#> 291                      NA                    <NA>                      NA
#> 292                      NA                    <NA>                      NA
#> 293                      NA                    <NA>                      NA
#> 294                      NA                    <NA>                      NA
#> 295                      NA                    <NA>                      NA
#> 296                      NA                    <NA>                      NA
#> 297                      NA                    <NA>                 8482671
#> 298                      NA                    <NA>                      NA
#> 299                      NA                    <NA>                      NA
#> 300                      NA                    <NA>                      NA
#> 301                      NA                    <NA>                      NA
#> 302                      NA                    <NA>                      NA
#> 303                      NA                    <NA>                      NA
#> 304                      NA                    <NA>                      NA
#> 305                      NA                    <NA>                      NA
#> 306                      NA                    <NA>                      NA
#> 307                      NA                    <NA>                      NA
#> 308                      NA                    <NA>                      NA
#> 309                      NA                    <NA>                      NA
#> 310                      NA              tv-timeout                      NA
#> 311                      NA                    <NA>                      NA
#> 312                      NA                    <NA>                      NA
#> 313                      NA                    <NA>                      NA
#> 314                      NA                    <NA>                      NA
#> 315                      NA                    <NA>                      NA
#> 316                      NA                    <NA>                      NA
#> 317                      NA                    <NA>                      NA
#> 318                      NA                    <NA>                      NA
#> 319                      NA                    <NA>                      NA
#> 320                      NA                    <NA>                      NA
#> 321                      NA                    <NA>                      NA
#> 322                      NA                    <NA>                      NA
#> 323                      NA              tv-timeout                      NA
#> 324                      NA                    <NA>                      NA
#> 325                      NA                    <NA>                      NA
#> 326                      NA                    <NA>                      NA
#> 327                      NA                    <NA>                      NA
#> 328                      NA                    <NA>                      NA
#> 329                      NA                    <NA>                      NA
#> 330                      NA                    <NA>                      NA
#> 331                      NA                    <NA>                      NA
#> 332                      NA                    <NA>                      NA
#> 333                      NA                    <NA>                      NA
#> 334                      NA                    <NA>                      NA
#> 335                      NA                    <NA>                 8481032
#> 336                      NA                    <NA>                      NA
#> 337                      NA                    <NA>                      NA
#> 338                      NA                    <NA>                      NA
#> 339                      NA                    <NA>                      NA
#> 340                      NA                    <NA>                      NA
#> 341                      NA                    <NA>                      NA
#> 342                      NA                    <NA>                      NA
#> 343                      NA                    <NA>                      NA
#> 344                      NA                    <NA>                      NA
#> 345                      NA                    <NA>                      NA
#> 346                      NA                    <NA>                      NA
#> 347                      NA                    <NA>                      NA
#> 348                      NA                    <NA>                      NA
#> 349                      NA                    <NA>                      NA
#>     details.scoringPlayerTotal details.assist1PlayerId
#> 1                           NA                      NA
#> 2                           NA                      NA
#> 3                           NA                      NA
#> 4                           NA                      NA
#> 5                           NA                      NA
#> 6                           NA                      NA
#> 7                           NA                      NA
#> 8                           NA                      NA
#> 9                           NA                      NA
#> 10                          NA                      NA
#> 11                          NA                      NA
#> 12                          NA                      NA
#> 13                          NA                      NA
#> 14                          NA                      NA
#> 15                          NA                      NA
#> 16                          NA                      NA
#> 17                          NA                      NA
#> 18                          NA                      NA
#> 19                          NA                      NA
#> 20                          NA                      NA
#> 21                          NA                      NA
#> 22                          NA                      NA
#> 23                          NA                      NA
#> 24                          NA                      NA
#> 25                          NA                      NA
#> 26                          NA                      NA
#> 27                          NA                      NA
#> 28                          NA                      NA
#> 29                          NA                      NA
#> 30                          NA                      NA
#> 31                          NA                      NA
#> 32                          NA                      NA
#> 33                          NA                      NA
#> 34                          NA                      NA
#> 35                          NA                      NA
#> 36                          NA                      NA
#> 37                          NA                      NA
#> 38                          NA                      NA
#> 39                          NA                      NA
#> 40                          NA                      NA
#> 41                          NA                      NA
#> 42                          NA                      NA
#> 43                          NA                      NA
#> 44                          NA                      NA
#> 45                          NA                      NA
#> 46                          NA                      NA
#> 47                          NA                      NA
#> 48                          NA                      NA
#> 49                          NA                      NA
#> 50                          NA                      NA
#> 51                          NA                      NA
#> 52                          NA                      NA
#> 53                          NA                      NA
#> 54                          NA                      NA
#> 55                          NA                      NA
#> 56                          NA                      NA
#> 57                           1                 8480192
#> 58                          NA                      NA
#> 59                          NA                      NA
#> 60                          NA                      NA
#> 61                          NA                      NA
#> 62                          NA                      NA
#> 63                          NA                      NA
#> 64                          NA                      NA
#> 65                          NA                      NA
#> 66                          NA                      NA
#> 67                          NA                      NA
#> 68                          NA                      NA
#> 69                          NA                      NA
#> 70                          NA                      NA
#> 71                          NA                      NA
#> 72                          NA                      NA
#> 73                          NA                      NA
#> 74                          NA                      NA
#> 75                          NA                      NA
#> 76                          NA                      NA
#> 77                          NA                      NA
#> 78                          NA                      NA
#> 79                          NA                      NA
#> 80                          NA                      NA
#> 81                          NA                      NA
#> 82                          NA                      NA
#> 83                          NA                      NA
#> 84                          NA                      NA
#> 85                          NA                      NA
#> 86                          NA                      NA
#> 87                          NA                      NA
#> 88                          NA                      NA
#> 89                          NA                      NA
#> 90                          NA                      NA
#> 91                          NA                      NA
#> 92                          NA                      NA
#> 93                           1                 8478399
#> 94                          NA                      NA
#> 95                          NA                      NA
#> 96                          NA                      NA
#> 97                          NA                      NA
#> 98                          NA                      NA
#> 99                          NA                      NA
#> 100                         NA                      NA
#> 101                         NA                      NA
#> 102                         NA                      NA
#> 103                         NA                      NA
#> 104                         NA                      NA
#> 105                         NA                      NA
#> 106                         NA                      NA
#> 107                         NA                      NA
#> 108                         NA                      NA
#> 109                         NA                      NA
#> 110                         NA                      NA
#> 111                         NA                      NA
#> 112                         NA                      NA
#> 113                         NA                      NA
#> 114                         NA                      NA
#> 115                         NA                      NA
#> 116                         NA                      NA
#> 117                         NA                      NA
#> 118                         NA                      NA
#> 119                         NA                      NA
#> 120                         NA                      NA
#> 121                         NA                      NA
#> 122                         NA                      NA
#> 123                         NA                      NA
#> 124                         NA                      NA
#> 125                         NA                      NA
#> 126                         NA                      NA
#> 127                         NA                      NA
#> 128                         NA                      NA
#> 129                         NA                      NA
#> 130                         NA                      NA
#> 131                         NA                      NA
#> 132                         NA                      NA
#> 133                         NA                      NA
#> 134                         NA                      NA
#> 135                         NA                      NA
#> 136                         NA                      NA
#> 137                         NA                      NA
#> 138                         NA                      NA
#> 139                         NA                      NA
#> 140                         NA                      NA
#> 141                         NA                      NA
#> 142                         NA                      NA
#> 143                         NA                      NA
#> 144                         NA                      NA
#> 145                          1                 8479414
#> 146                         NA                      NA
#> 147                         NA                      NA
#> 148                         NA                      NA
#> 149                         NA                      NA
#> 150                         NA                      NA
#> 151                         NA                      NA
#> 152                         NA                      NA
#> 153                         NA                      NA
#> 154                         NA                      NA
#> 155                         NA                      NA
#> 156                         NA                      NA
#> 157                         NA                      NA
#> 158                         NA                      NA
#> 159                         NA                      NA
#> 160                         NA                      NA
#> 161                         NA                      NA
#> 162                         NA                      NA
#> 163                         NA                      NA
#> 164                         NA                      NA
#> 165                         NA                      NA
#> 166                         NA                      NA
#> 167                         NA                      NA
#> 168                         NA                      NA
#> 169                         NA                      NA
#> 170                         NA                      NA
#> 171                         NA                      NA
#> 172                         NA                      NA
#> 173                         NA                      NA
#> 174                         NA                      NA
#> 175                         NA                      NA
#> 176                         NA                      NA
#> 177                         NA                      NA
#> 178                         NA                      NA
#> 179                         NA                      NA
#> 180                         NA                      NA
#> 181                         NA                      NA
#> 182                         NA                      NA
#> 183                         NA                      NA
#> 184                         NA                      NA
#> 185                         NA                      NA
#> 186                         NA                      NA
#> 187                         NA                      NA
#> 188                         NA                      NA
#> 189                         NA                      NA
#> 190                         NA                      NA
#> 191                         NA                      NA
#> 192                         NA                      NA
#> 193                         NA                      NA
#> 194                         NA                      NA
#> 195                         NA                      NA
#> 196                         NA                      NA
#> 197                         NA                      NA
#> 198                         NA                      NA
#> 199                         NA                      NA
#> 200                         NA                      NA
#> 201                         NA                      NA
#> 202                         NA                      NA
#> 203                         NA                      NA
#> 204                         NA                      NA
#> 205                         NA                      NA
#> 206                         NA                      NA
#> 207                         NA                      NA
#> 208                         NA                      NA
#> 209                         NA                      NA
#> 210                         NA                      NA
#> 211                         NA                      NA
#> 212                         NA                      NA
#> 213                         NA                      NA
#> 214                         NA                      NA
#> 215                         NA                      NA
#> 216                         NA                      NA
#> 217                         NA                      NA
#> 218                         NA                      NA
#> 219                         NA                      NA
#> 220                         NA                      NA
#> 221                         NA                      NA
#> 222                         NA                      NA
#> 223                         NA                      NA
#> 224                         NA                      NA
#> 225                         NA                      NA
#> 226                         NA                      NA
#> 227                         NA                      NA
#> 228                         NA                      NA
#> 229                         NA                      NA
#> 230                         NA                      NA
#> 231                         NA                      NA
#> 232                         NA                      NA
#> 233                         NA                      NA
#> 234                         NA                      NA
#> 235                         NA                      NA
#> 236                         NA                      NA
#> 237                         NA                      NA
#> 238                         NA                      NA
#> 239                         NA                      NA
#> 240                         NA                      NA
#> 241                         NA                      NA
#> 242                         NA                      NA
#> 243                         NA                      NA
#> 244                         NA                      NA
#> 245                         NA                      NA
#> 246                         NA                      NA
#> 247                         NA                      NA
#> 248                         NA                      NA
#> 249                         NA                      NA
#> 250                         NA                      NA
#> 251                         NA                      NA
#> 252                         NA                      NA
#> 253                         NA                      NA
#> 254                         NA                      NA
#> 255                         NA                      NA
#> 256                         NA                      NA
#> 257                         NA                      NA
#> 258                         NA                      NA
#> 259                         NA                      NA
#> 260                         NA                      NA
#> 261                         NA                      NA
#> 262                         NA                      NA
#> 263                         NA                      NA
#> 264                         NA                      NA
#> 265                         NA                      NA
#> 266                         NA                      NA
#> 267                         NA                      NA
#> 268                         NA                      NA
#> 269                         NA                      NA
#> 270                         NA                      NA
#> 271                         NA                      NA
#> 272                         NA                      NA
#> 273                         NA                      NA
#> 274                         NA                      NA
#> 275                         NA                      NA
#> 276                         NA                      NA
#> 277                         NA                      NA
#> 278                         NA                      NA
#> 279                         NA                      NA
#> 280                         NA                      NA
#> 281                         NA                      NA
#> 282                         NA                      NA
#> 283                         NA                      NA
#> 284                         NA                      NA
#> 285                         NA                      NA
#> 286                         NA                      NA
#> 287                         NA                      NA
#> 288                         NA                      NA
#> 289                         NA                      NA
#> 290                         NA                      NA
#> 291                         NA                      NA
#> 292                         NA                      NA
#> 293                         NA                      NA
#> 294                         NA                      NA
#> 295                         NA                      NA
#> 296                         NA                      NA
#> 297                          1                 8482175
#> 298                         NA                      NA
#> 299                         NA                      NA
#> 300                         NA                      NA
#> 301                         NA                      NA
#> 302                         NA                      NA
#> 303                         NA                      NA
#> 304                         NA                      NA
#> 305                         NA                      NA
#> 306                         NA                      NA
#> 307                         NA                      NA
#> 308                         NA                      NA
#> 309                         NA                      NA
#> 310                         NA                      NA
#> 311                         NA                      NA
#> 312                         NA                      NA
#> 313                         NA                      NA
#> 314                         NA                      NA
#> 315                         NA                      NA
#> 316                         NA                      NA
#> 317                         NA                      NA
#> 318                         NA                      NA
#> 319                         NA                      NA
#> 320                         NA                      NA
#> 321                         NA                      NA
#> 322                         NA                      NA
#> 323                         NA                      NA
#> 324                         NA                      NA
#> 325                         NA                      NA
#> 326                         NA                      NA
#> 327                         NA                      NA
#> 328                         NA                      NA
#> 329                         NA                      NA
#> 330                         NA                      NA
#> 331                         NA                      NA
#> 332                         NA                      NA
#> 333                         NA                      NA
#> 334                         NA                      NA
#> 335                          1                 8479414
#> 336                         NA                      NA
#> 337                         NA                      NA
#> 338                         NA                      NA
#> 339                         NA                      NA
#> 340                         NA                      NA
#> 341                         NA                      NA
#> 342                         NA                      NA
#> 343                         NA                      NA
#> 344                         NA                      NA
#> 345                         NA                      NA
#> 346                         NA                      NA
#> 347                         NA                      NA
#> 348                         NA                      NA
#> 349                         NA                      NA
#>     details.assist1PlayerTotal details.awayScore details.homeScore
#> 1                           NA                NA                NA
#> 2                           NA                NA                NA
#> 3                           NA                NA                NA
#> 4                           NA                NA                NA
#> 5                           NA                NA                NA
#> 6                           NA                NA                NA
#> 7                           NA                NA                NA
#> 8                           NA                NA                NA
#> 9                           NA                NA                NA
#> 10                          NA                NA                NA
#> 11                          NA                NA                NA
#> 12                          NA                NA                NA
#> 13                          NA                NA                NA
#> 14                          NA                NA                NA
#> 15                          NA                NA                NA
#> 16                          NA                NA                NA
#> 17                          NA                NA                NA
#> 18                          NA                NA                NA
#> 19                          NA                NA                NA
#> 20                          NA                NA                NA
#> 21                          NA                NA                NA
#> 22                          NA                NA                NA
#> 23                          NA                NA                NA
#> 24                          NA                NA                NA
#> 25                          NA                NA                NA
#> 26                          NA                NA                NA
#> 27                          NA                NA                NA
#> 28                          NA                NA                NA
#> 29                          NA                NA                NA
#> 30                          NA                NA                NA
#> 31                          NA                NA                NA
#> 32                          NA                NA                NA
#> 33                          NA                NA                NA
#> 34                          NA                NA                NA
#> 35                          NA                NA                NA
#> 36                          NA                NA                NA
#> 37                          NA                NA                NA
#> 38                          NA                NA                NA
#> 39                          NA                NA                NA
#> 40                          NA                NA                NA
#> 41                          NA                NA                NA
#> 42                          NA                NA                NA
#> 43                          NA                NA                NA
#> 44                          NA                NA                NA
#> 45                          NA                NA                NA
#> 46                          NA                NA                NA
#> 47                          NA                NA                NA
#> 48                          NA                NA                NA
#> 49                          NA                NA                NA
#> 50                          NA                NA                NA
#> 51                          NA                NA                NA
#> 52                          NA                NA                NA
#> 53                          NA                NA                NA
#> 54                          NA                NA                NA
#> 55                          NA                NA                NA
#> 56                          NA                NA                NA
#> 57                           1                 1                 0
#> 58                          NA                NA                NA
#> 59                          NA                NA                NA
#> 60                          NA                NA                NA
#> 61                          NA                NA                NA
#> 62                          NA                NA                NA
#> 63                          NA                NA                NA
#> 64                          NA                NA                NA
#> 65                          NA                NA                NA
#> 66                          NA                NA                NA
#> 67                          NA                NA                NA
#> 68                          NA                NA                NA
#> 69                          NA                NA                NA
#> 70                          NA                NA                NA
#> 71                          NA                NA                NA
#> 72                          NA                NA                NA
#> 73                          NA                NA                NA
#> 74                          NA                NA                NA
#> 75                          NA                NA                NA
#> 76                          NA                NA                NA
#> 77                          NA                NA                NA
#> 78                          NA                NA                NA
#> 79                          NA                NA                NA
#> 80                          NA                NA                NA
#> 81                          NA                NA                NA
#> 82                          NA                NA                NA
#> 83                          NA                NA                NA
#> 84                          NA                NA                NA
#> 85                          NA                NA                NA
#> 86                          NA                NA                NA
#> 87                          NA                NA                NA
#> 88                          NA                NA                NA
#> 89                          NA                NA                NA
#> 90                          NA                NA                NA
#> 91                          NA                NA                NA
#> 92                          NA                NA                NA
#> 93                           1                 2                 0
#> 94                          NA                NA                NA
#> 95                          NA                NA                NA
#> 96                          NA                NA                NA
#> 97                          NA                NA                NA
#> 98                          NA                NA                NA
#> 99                          NA                NA                NA
#> 100                         NA                NA                NA
#> 101                         NA                NA                NA
#> 102                         NA                NA                NA
#> 103                         NA                NA                NA
#> 104                         NA                NA                NA
#> 105                         NA                NA                NA
#> 106                         NA                NA                NA
#> 107                         NA                NA                NA
#> 108                         NA                NA                NA
#> 109                         NA                NA                NA
#> 110                         NA                NA                NA
#> 111                         NA                NA                NA
#> 112                         NA                NA                NA
#> 113                         NA                NA                NA
#> 114                         NA                NA                NA
#> 115                         NA                NA                NA
#> 116                         NA                NA                NA
#> 117                         NA                NA                NA
#> 118                         NA                NA                NA
#> 119                         NA                NA                NA
#> 120                         NA                NA                NA
#> 121                         NA                NA                NA
#> 122                         NA                NA                NA
#> 123                         NA                NA                NA
#> 124                         NA                NA                NA
#> 125                         NA                NA                NA
#> 126                         NA                NA                NA
#> 127                         NA                NA                NA
#> 128                         NA                NA                NA
#> 129                         NA                NA                NA
#> 130                         NA                NA                NA
#> 131                         NA                NA                NA
#> 132                         NA                NA                NA
#> 133                         NA                NA                NA
#> 134                         NA                NA                NA
#> 135                         NA                NA                NA
#> 136                         NA                NA                NA
#> 137                         NA                NA                NA
#> 138                         NA                NA                NA
#> 139                         NA                NA                NA
#> 140                         NA                NA                NA
#> 141                         NA                NA                NA
#> 142                         NA                NA                NA
#> 143                         NA                NA                NA
#> 144                         NA                NA                NA
#> 145                          1                 3                 0
#> 146                         NA                NA                NA
#> 147                         NA                NA                NA
#> 148                         NA                NA                NA
#> 149                         NA                NA                NA
#> 150                         NA                NA                NA
#> 151                         NA                NA                NA
#> 152                         NA                NA                NA
#> 153                         NA                NA                NA
#> 154                         NA                NA                NA
#> 155                         NA                NA                NA
#> 156                         NA                NA                NA
#> 157                         NA                NA                NA
#> 158                         NA                NA                NA
#> 159                         NA                NA                NA
#> 160                         NA                NA                NA
#> 161                         NA                NA                NA
#> 162                         NA                NA                NA
#> 163                         NA                NA                NA
#> 164                         NA                NA                NA
#> 165                         NA                NA                NA
#> 166                         NA                NA                NA
#> 167                         NA                NA                NA
#> 168                         NA                NA                NA
#> 169                         NA                NA                NA
#> 170                         NA                NA                NA
#> 171                         NA                NA                NA
#> 172                         NA                NA                NA
#> 173                         NA                NA                NA
#> 174                         NA                NA                NA
#> 175                         NA                NA                NA
#> 176                         NA                NA                NA
#> 177                         NA                NA                NA
#> 178                         NA                NA                NA
#> 179                         NA                NA                NA
#> 180                         NA                NA                NA
#> 181                         NA                NA                NA
#> 182                         NA                NA                NA
#> 183                         NA                NA                NA
#> 184                         NA                NA                NA
#> 185                         NA                NA                NA
#> 186                         NA                NA                NA
#> 187                         NA                NA                NA
#> 188                         NA                NA                NA
#> 189                         NA                NA                NA
#> 190                         NA                NA                NA
#> 191                         NA                NA                NA
#> 192                         NA                NA                NA
#> 193                         NA                NA                NA
#> 194                         NA                NA                NA
#> 195                         NA                NA                NA
#> 196                         NA                NA                NA
#> 197                         NA                NA                NA
#> 198                         NA                NA                NA
#> 199                         NA                NA                NA
#> 200                         NA                NA                NA
#> 201                         NA                NA                NA
#> 202                         NA                NA                NA
#> 203                         NA                NA                NA
#> 204                         NA                NA                NA
#> 205                         NA                NA                NA
#> 206                         NA                NA                NA
#> 207                         NA                NA                NA
#> 208                         NA                NA                NA
#> 209                         NA                NA                NA
#> 210                         NA                NA                NA
#> 211                         NA                NA                NA
#> 212                         NA                NA                NA
#> 213                         NA                NA                NA
#> 214                         NA                NA                NA
#> 215                         NA                NA                NA
#> 216                         NA                NA                NA
#> 217                         NA                NA                NA
#> 218                         NA                NA                NA
#> 219                         NA                NA                NA
#> 220                         NA                NA                NA
#> 221                         NA                NA                NA
#> 222                         NA                NA                NA
#> 223                         NA                NA                NA
#> 224                         NA                NA                NA
#> 225                         NA                NA                NA
#> 226                         NA                NA                NA
#> 227                         NA                NA                NA
#> 228                         NA                NA                NA
#> 229                         NA                NA                NA
#> 230                         NA                NA                NA
#> 231                         NA                NA                NA
#> 232                         NA                NA                NA
#> 233                         NA                NA                NA
#> 234                         NA                NA                NA
#> 235                         NA                NA                NA
#> 236                         NA                NA                NA
#> 237                         NA                NA                NA
#> 238                         NA                NA                NA
#> 239                         NA                NA                NA
#> 240                         NA                NA                NA
#> 241                         NA                NA                NA
#> 242                         NA                NA                NA
#> 243                         NA                NA                NA
#> 244                         NA                NA                NA
#> 245                         NA                NA                NA
#> 246                         NA                NA                NA
#> 247                         NA                NA                NA
#> 248                         NA                NA                NA
#> 249                         NA                NA                NA
#> 250                         NA                NA                NA
#> 251                         NA                NA                NA
#> 252                         NA                NA                NA
#> 253                         NA                NA                NA
#> 254                         NA                NA                NA
#> 255                         NA                NA                NA
#> 256                         NA                NA                NA
#> 257                         NA                NA                NA
#> 258                         NA                NA                NA
#> 259                         NA                NA                NA
#> 260                         NA                NA                NA
#> 261                         NA                NA                NA
#> 262                         NA                NA                NA
#> 263                         NA                NA                NA
#> 264                         NA                NA                NA
#> 265                         NA                NA                NA
#> 266                         NA                NA                NA
#> 267                         NA                NA                NA
#> 268                         NA                NA                NA
#> 269                         NA                NA                NA
#> 270                         NA                NA                NA
#> 271                         NA                NA                NA
#> 272                         NA                NA                NA
#> 273                         NA                NA                NA
#> 274                         NA                NA                NA
#> 275                         NA                NA                NA
#> 276                         NA                NA                NA
#> 277                         NA                NA                NA
#> 278                         NA                NA                NA
#> 279                         NA                NA                NA
#> 280                         NA                NA                NA
#> 281                         NA                NA                NA
#> 282                         NA                NA                NA
#> 283                         NA                NA                NA
#> 284                         NA                NA                NA
#> 285                         NA                NA                NA
#> 286                         NA                NA                NA
#> 287                         NA                NA                NA
#> 288                         NA                NA                NA
#> 289                         NA                NA                NA
#> 290                         NA                NA                NA
#> 291                         NA                NA                NA
#> 292                         NA                NA                NA
#> 293                         NA                NA                NA
#> 294                         NA                NA                NA
#> 295                         NA                NA                NA
#> 296                         NA                NA                NA
#> 297                          1                 3                 1
#> 298                         NA                NA                NA
#> 299                         NA                NA                NA
#> 300                         NA                NA                NA
#> 301                         NA                NA                NA
#> 302                         NA                NA                NA
#> 303                         NA                NA                NA
#> 304                         NA                NA                NA
#> 305                         NA                NA                NA
#> 306                         NA                NA                NA
#> 307                         NA                NA                NA
#> 308                         NA                NA                NA
#> 309                         NA                NA                NA
#> 310                         NA                NA                NA
#> 311                         NA                NA                NA
#> 312                         NA                NA                NA
#> 313                         NA                NA                NA
#> 314                         NA                NA                NA
#> 315                         NA                NA                NA
#> 316                         NA                NA                NA
#> 317                         NA                NA                NA
#> 318                         NA                NA                NA
#> 319                         NA                NA                NA
#> 320                         NA                NA                NA
#> 321                         NA                NA                NA
#> 322                         NA                NA                NA
#> 323                         NA                NA                NA
#> 324                         NA                NA                NA
#> 325                         NA                NA                NA
#> 326                         NA                NA                NA
#> 327                         NA                NA                NA
#> 328                         NA                NA                NA
#> 329                         NA                NA                NA
#> 330                         NA                NA                NA
#> 331                         NA                NA                NA
#> 332                         NA                NA                NA
#> 333                         NA                NA                NA
#> 334                         NA                NA                NA
#> 335                          2                 4                 1
#> 336                         NA                NA                NA
#> 337                         NA                NA                NA
#> 338                         NA                NA                NA
#> 339                         NA                NA                NA
#> 340                         NA                NA                NA
#> 341                         NA                NA                NA
#> 342                         NA                NA                NA
#> 343                         NA                NA                NA
#> 344                         NA                NA                NA
#> 345                         NA                NA                NA
#> 346                         NA                NA                NA
#> 347                         NA                NA                NA
#> 348                         NA                NA                NA
#> 349                         NA                NA                NA
#>                                                                   details.highlightClipSharingUrl
#> 1                                                                                            <NA>
#> 2                                                                                            <NA>
#> 3                                                                                            <NA>
#> 4                                                                                            <NA>
#> 5                                                                                            <NA>
#> 6                                                                                            <NA>
#> 7                                                                                            <NA>
#> 8                                                                                            <NA>
#> 9                                                                                            <NA>
#> 10                                                                                           <NA>
#> 11                                                                                           <NA>
#> 12                                                                                           <NA>
#> 13                                                                                           <NA>
#> 14                                                                                           <NA>
#> 15                                                                                           <NA>
#> 16                                                                                           <NA>
#> 17                                                                                           <NA>
#> 18                                                                                           <NA>
#> 19                                                                                           <NA>
#> 20                                                                                           <NA>
#> 21                                                                                           <NA>
#> 22                                                                                           <NA>
#> 23                                                                                           <NA>
#> 24                                                                                           <NA>
#> 25                                                                                           <NA>
#> 26                                                                                           <NA>
#> 27                                                                                           <NA>
#> 28                                                                                           <NA>
#> 29                                                                                           <NA>
#> 30                                                                                           <NA>
#> 31                                                                                           <NA>
#> 32                                                                                           <NA>
#> 33                                                                                           <NA>
#> 34                                                                                           <NA>
#> 35                                                                                           <NA>
#> 36                                                                                           <NA>
#> 37                                                                                           <NA>
#> 38                                                                                           <NA>
#> 39                                                                                           <NA>
#> 40                                                                                           <NA>
#> 41                                                                                           <NA>
#> 42                                                                                           <NA>
#> 43                                                                                           <NA>
#> 44                                                                                           <NA>
#> 45                                                                                           <NA>
#> 46                                                                                           <NA>
#> 47                                                                                           <NA>
#> 48                                                                                           <NA>
#> 49                                                                                           <NA>
#> 50                                                                                           <NA>
#> 51                                                                                           <NA>
#> 52                                                                                           <NA>
#> 53                                                                                           <NA>
#> 54                                                                                           <NA>
#> 55                                                                                           <NA>
#> 56                                                                                           <NA>
#> 57    https://nhl.com/video/njd-buf-noesen-scores-goal-against-ukko-pekka-luukkonen-6362848229112
#> 58                                                                                           <NA>
#> 59                                                                                           <NA>
#> 60                                                                                           <NA>
#> 61                                                                                           <NA>
#> 62                                                                                           <NA>
#> 63                                                                                           <NA>
#> 64                                                                                           <NA>
#> 65                                                                                           <NA>
#> 66                                                                                           <NA>
#> 67                                                                                           <NA>
#> 68                                                                                           <NA>
#> 69                                                                                           <NA>
#> 70                                                                                           <NA>
#> 71                                                                                           <NA>
#> 72                                                                                           <NA>
#> 73                                                                                           <NA>
#> 74                                                                                           <NA>
#> 75                                                                                           <NA>
#> 76                                                                                           <NA>
#> 77                                                                                           <NA>
#> 78                                                                                           <NA>
#> 79                                                                                           <NA>
#> 80                                                                                           <NA>
#> 81                                                                                           <NA>
#> 82                                                                                           <NA>
#> 83                                                                                           <NA>
#> 84                                                                                           <NA>
#> 85                                                                                           <NA>
#> 86                                                                                           <NA>
#> 87                                                                                           <NA>
#> 88                                                                                           <NA>
#> 89                                                                                           <NA>
#> 90                                                                                           <NA>
#> 91                                                                                           <NA>
#> 92                                                                                           <NA>
#> 93  https://nhl.com/video/njd-buf-kovacevic-scores-ppg-against-ukko-pekka-luukkonen-6362847541112
#> 94                                                                                           <NA>
#> 95                                                                                           <NA>
#> 96                                                                                           <NA>
#> 97                                                                                           <NA>
#> 98                                                                                           <NA>
#> 99                                                                                           <NA>
#> 100                                                                                          <NA>
#> 101                                                                                          <NA>
#> 102                                                                                          <NA>
#> 103                                                                                          <NA>
#> 104                                                                                          <NA>
#> 105                                                                                          <NA>
#> 106                                                                                          <NA>
#> 107                                                                                          <NA>
#> 108                                                                                          <NA>
#> 109                                                                                          <NA>
#> 110                                                                                          <NA>
#> 111                                                                                          <NA>
#> 112                                                                                          <NA>
#> 113                                                                                          <NA>
#> 114                                                                                          <NA>
#> 115                                                                                          <NA>
#> 116                                                                                          <NA>
#> 117                                                                                          <NA>
#> 118                                                                                          <NA>
#> 119                                                                                          <NA>
#> 120                                                                                          <NA>
#> 121                                                                                          <NA>
#> 122                                                                                          <NA>
#> 123                                                                                          <NA>
#> 124                                                                                          <NA>
#> 125                                                                                          <NA>
#> 126                                                                                          <NA>
#> 127                                                                                          <NA>
#> 128                                                                                          <NA>
#> 129                                                                                          <NA>
#> 130                                                                                          <NA>
#> 131                                                                                          <NA>
#> 132                                                                                          <NA>
#> 133                                                                                          <NA>
#> 134                                                                                          <NA>
#> 135                                                                                          <NA>
#> 136                                                                                          <NA>
#> 137                                                                                          <NA>
#> 138                                                                                          <NA>
#> 139                                                                                          <NA>
#> 140                                                                                          <NA>
#> 141                                                                                          <NA>
#> 142                                                                                          <NA>
#> 143                                                                                          <NA>
#> 144                                                                                          <NA>
#> 145  https://nhl.com/video/njd-buf-hischier-scores-ppg-against-ukko-pekka-luukkonen-6362848582112
#> 146                                                                                          <NA>
#> 147                                                                                          <NA>
#> 148                                                                                          <NA>
#> 149                                                                                          <NA>
#> 150                                                                                          <NA>
#> 151                                                                                          <NA>
#> 152                                                                                          <NA>
#> 153                                                                                          <NA>
#> 154                                                                                          <NA>
#> 155                                                                                          <NA>
#> 156                                                                                          <NA>
#> 157                                                                                          <NA>
#> 158                                                                                          <NA>
#> 159                                                                                          <NA>
#> 160                                                                                          <NA>
#> 161                                                                                          <NA>
#> 162                                                                                          <NA>
#> 163                                                                                          <NA>
#> 164                                                                                          <NA>
#> 165                                                                                          <NA>
#> 166                                                                                          <NA>
#> 167                                                                                          <NA>
#> 168                                                                                          <NA>
#> 169                                                                                          <NA>
#> 170                                                                                          <NA>
#> 171                                                                                          <NA>
#> 172                                                                                          <NA>
#> 173                                                                                          <NA>
#> 174                                                                                          <NA>
#> 175                                                                                          <NA>
#> 176                                                                                          <NA>
#> 177                                                                                          <NA>
#> 178                                                                                          <NA>
#> 179                                                                                          <NA>
#> 180                                                                                          <NA>
#> 181                                                                                          <NA>
#> 182                                                                                          <NA>
#> 183                                                                                          <NA>
#> 184                                                                                          <NA>
#> 185                                                                                          <NA>
#> 186                                                                                          <NA>
#> 187                                                                                          <NA>
#> 188                                                                                          <NA>
#> 189                                                                                          <NA>
#> 190                                                                                          <NA>
#> 191                                                                                          <NA>
#> 192                                                                                          <NA>
#> 193                                                                                          <NA>
#> 194                                                                                          <NA>
#> 195                                                                                          <NA>
#> 196                                                                                          <NA>
#> 197                                                                                          <NA>
#> 198                                                                                          <NA>
#> 199                                                                                          <NA>
#> 200                                                                                          <NA>
#> 201                                                                                          <NA>
#> 202                                                                                          <NA>
#> 203                                                                                          <NA>
#> 204                                                                                          <NA>
#> 205                                                                                          <NA>
#> 206                                                                                          <NA>
#> 207                                                                                          <NA>
#> 208                                                                                          <NA>
#> 209                                                                                          <NA>
#> 210                                                                                          <NA>
#> 211                                                                                          <NA>
#> 212                                                                                          <NA>
#> 213                                                                                          <NA>
#> 214                                                                                          <NA>
#> 215                                                                                          <NA>
#> 216                                                                                          <NA>
#> 217                                                                                          <NA>
#> 218                                                                                          <NA>
#> 219                                                                                          <NA>
#> 220                                                                                          <NA>
#> 221                                                                                          <NA>
#> 222                                                                                          <NA>
#> 223                                                                                          <NA>
#> 224                                                                                          <NA>
#> 225                                                                                          <NA>
#> 226                                                                                          <NA>
#> 227                                                                                          <NA>
#> 228                                                                                          <NA>
#> 229                                                                                          <NA>
#> 230                                                                                          <NA>
#> 231                                                                                          <NA>
#> 232                                                                                          <NA>
#> 233                                                                                          <NA>
#> 234                                                                                          <NA>
#> 235                                                                                          <NA>
#> 236                                                                                          <NA>
#> 237                                                                                          <NA>
#> 238                                                                                          <NA>
#> 239                                                                                          <NA>
#> 240                                                                                          <NA>
#> 241                                                                                          <NA>
#> 242                                                                                          <NA>
#> 243                                                                                          <NA>
#> 244                                                                                          <NA>
#> 245                                                                                          <NA>
#> 246                                                                                          <NA>
#> 247                                                                                          <NA>
#> 248                                                                                          <NA>
#> 249                                                                                          <NA>
#> 250                                                                                          <NA>
#> 251                                                                                          <NA>
#> 252                                                                                          <NA>
#> 253                                                                                          <NA>
#> 254                                                                                          <NA>
#> 255                                                                                          <NA>
#> 256                                                                                          <NA>
#> 257                                                                                          <NA>
#> 258                                                                                          <NA>
#> 259                                                                                          <NA>
#> 260                                                                                          <NA>
#> 261                                                                                          <NA>
#> 262                                                                                          <NA>
#> 263                                                                                          <NA>
#> 264                                                                                          <NA>
#> 265                                                                                          <NA>
#> 266                                                                                          <NA>
#> 267                                                                                          <NA>
#> 268                                                                                          <NA>
#> 269                                                                                          <NA>
#> 270                                                                                          <NA>
#> 271                                                                                          <NA>
#> 272                                                                                          <NA>
#> 273                                                                                          <NA>
#> 274                                                                                          <NA>
#> 275                                                                                          <NA>
#> 276                                                                                          <NA>
#> 277                                                                                          <NA>
#> 278                                                                                          <NA>
#> 279                                                                                          <NA>
#> 280                                                                                          <NA>
#> 281                                                                                          <NA>
#> 282                                                                                          <NA>
#> 283                                                                                          <NA>
#> 284                                                                                          <NA>
#> 285                                                                                          <NA>
#> 286                                                                                          <NA>
#> 287                                                                                          <NA>
#> 288                                                                                          <NA>
#> 289                                                                                          <NA>
#> 290                                                                                          <NA>
#> 291                                                                                          <NA>
#> 292                                                                                          <NA>
#> 293                                                                                          <NA>
#> 294                                                                                          <NA>
#> 295                                                                                          <NA>
#> 296                                                                                          <NA>
#> 297          https://nhl.com/video/njd-buf-power-scores-shg-against-jacob-markstrom-6362851617112
#> 298                                                                                          <NA>
#> 299                                                                                          <NA>
#> 300                                                                                          <NA>
#> 301                                                                                          <NA>
#> 302                                                                                          <NA>
#> 303                                                                                          <NA>
#> 304                                                                                          <NA>
#> 305                                                                                          <NA>
#> 306                                                                                          <NA>
#> 307                                                                                          <NA>
#> 308                                                                                          <NA>
#> 309                                                                                          <NA>
#> 310                                                                                          <NA>
#> 311                                                                                          <NA>
#> 312                                                                                          <NA>
#> 313                                                                                          <NA>
#> 314                                                                                          <NA>
#> 315                                                                                          <NA>
#> 316                                                                                          <NA>
#> 317                                                                                          <NA>
#> 318                                                                                          <NA>
#> 319                                                                                          <NA>
#> 320                                                                                          <NA>
#> 321                                                                                          <NA>
#> 322                                                                                          <NA>
#> 323                                                                                          <NA>
#> 324                                                                                          <NA>
#> 325                                                                                          <NA>
#> 326                                                                                          <NA>
#> 327                                                                                          <NA>
#> 328                                                                                          <NA>
#> 329                                                                                          <NA>
#> 330                                                                                          <NA>
#> 331                                                                                          <NA>
#> 332                                                                                          <NA>
#> 333                                                                                          <NA>
#> 334                                                                                          <NA>
#> 335         https://nhl.com/video/njd-buf-cotter-scores-goal-against-buffalo-sabres-6362851720112
#> 336                                                                                          <NA>
#> 337                                                                                          <NA>
#> 338                                                                                          <NA>
#> 339                                                                                          <NA>
#> 340                                                                                          <NA>
#> 341                                                                                          <NA>
#> 342                                                                                          <NA>
#> 343                                                                                          <NA>
#> 344                                                                                          <NA>
#> 345                                                                                          <NA>
#> 346                                                                                          <NA>
#> 347                                                                                          <NA>
#> 348                                                                                          <NA>
#> 349                                                                                          <NA>
#>     details.highlightClip details.discreteClip details.assist2PlayerId
#> 1                      NA                   NA                      NA
#> 2                      NA                   NA                      NA
#> 3                      NA                   NA                      NA
#> 4                      NA                   NA                      NA
#> 5                      NA                   NA                      NA
#> 6                      NA                   NA                      NA
#> 7                      NA                   NA                      NA
#> 8                      NA                   NA                      NA
#> 9                      NA                   NA                      NA
#> 10                     NA                   NA                      NA
#> 11                     NA                   NA                      NA
#> 12                     NA                   NA                      NA
#> 13                     NA                   NA                      NA
#> 14                     NA                   NA                      NA
#> 15                     NA                   NA                      NA
#> 16                     NA                   NA                      NA
#> 17                     NA                   NA                      NA
#> 18                     NA                   NA                      NA
#> 19                     NA                   NA                      NA
#> 20                     NA                   NA                      NA
#> 21                     NA                   NA                      NA
#> 22                     NA                   NA                      NA
#> 23                     NA                   NA                      NA
#> 24                     NA                   NA                      NA
#> 25                     NA                   NA                      NA
#> 26                     NA                   NA                      NA
#> 27                     NA                   NA                      NA
#> 28                     NA                   NA                      NA
#> 29                     NA                   NA                      NA
#> 30                     NA                   NA                      NA
#> 31                     NA                   NA                      NA
#> 32                     NA                   NA                      NA
#> 33                     NA                   NA                      NA
#> 34                     NA                   NA                      NA
#> 35                     NA                   NA                      NA
#> 36                     NA                   NA                      NA
#> 37                     NA                   NA                      NA
#> 38                     NA                   NA                      NA
#> 39                     NA                   NA                      NA
#> 40                     NA                   NA                      NA
#> 41                     NA                   NA                      NA
#> 42                     NA                   NA                      NA
#> 43                     NA                   NA                      NA
#> 44                     NA                   NA                      NA
#> 45                     NA                   NA                      NA
#> 46                     NA                   NA                      NA
#> 47                     NA                   NA                      NA
#> 48                     NA                   NA                      NA
#> 49                     NA                   NA                      NA
#> 50                     NA                   NA                      NA
#> 51                     NA                   NA                      NA
#> 52                     NA                   NA                      NA
#> 53                     NA                   NA                      NA
#> 54                     NA                   NA                      NA
#> 55                     NA                   NA                      NA
#> 56                     NA                   NA                      NA
#> 57           6.362848e+12         6.362846e+12                      NA
#> 58                     NA                   NA                      NA
#> 59                     NA                   NA                      NA
#> 60                     NA                   NA                      NA
#> 61                     NA                   NA                      NA
#> 62                     NA                   NA                      NA
#> 63                     NA                   NA                      NA
#> 64                     NA                   NA                      NA
#> 65                     NA                   NA                      NA
#> 66                     NA                   NA                      NA
#> 67                     NA                   NA                      NA
#> 68                     NA                   NA                      NA
#> 69                     NA                   NA                      NA
#> 70                     NA                   NA                      NA
#> 71                     NA                   NA                      NA
#> 72                     NA                   NA                      NA
#> 73                     NA                   NA                      NA
#> 74                     NA                   NA                      NA
#> 75                     NA                   NA                      NA
#> 76                     NA                   NA                      NA
#> 77                     NA                   NA                      NA
#> 78                     NA                   NA                      NA
#> 79                     NA                   NA                      NA
#> 80                     NA                   NA                      NA
#> 81                     NA                   NA                      NA
#> 82                     NA                   NA                      NA
#> 83                     NA                   NA                      NA
#> 84                     NA                   NA                      NA
#> 85                     NA                   NA                      NA
#> 86                     NA                   NA                      NA
#> 87                     NA                   NA                      NA
#> 88                     NA                   NA                      NA
#> 89                     NA                   NA                      NA
#> 90                     NA                   NA                      NA
#> 91                     NA                   NA                      NA
#> 92                     NA                   NA                      NA
#> 93           6.362848e+12         6.362849e+12                 8482110
#> 94                     NA                   NA                      NA
#> 95                     NA                   NA                      NA
#> 96                     NA                   NA                      NA
#> 97                     NA                   NA                      NA
#> 98                     NA                   NA                      NA
#> 99                     NA                   NA                      NA
#> 100                    NA                   NA                      NA
#> 101                    NA                   NA                      NA
#> 102                    NA                   NA                      NA
#> 103                    NA                   NA                      NA
#> 104                    NA                   NA                      NA
#> 105                    NA                   NA                      NA
#> 106                    NA                   NA                      NA
#> 107                    NA                   NA                      NA
#> 108                    NA                   NA                      NA
#> 109                    NA                   NA                      NA
#> 110                    NA                   NA                      NA
#> 111                    NA                   NA                      NA
#> 112                    NA                   NA                      NA
#> 113                    NA                   NA                      NA
#> 114                    NA                   NA                      NA
#> 115                    NA                   NA                      NA
#> 116                    NA                   NA                      NA
#> 117                    NA                   NA                      NA
#> 118                    NA                   NA                      NA
#> 119                    NA                   NA                      NA
#> 120                    NA                   NA                      NA
#> 121                    NA                   NA                      NA
#> 122                    NA                   NA                      NA
#> 123                    NA                   NA                      NA
#> 124                    NA                   NA                      NA
#> 125                    NA                   NA                      NA
#> 126                    NA                   NA                      NA
#> 127                    NA                   NA                      NA
#> 128                    NA                   NA                      NA
#> 129                    NA                   NA                      NA
#> 130                    NA                   NA                      NA
#> 131                    NA                   NA                      NA
#> 132                    NA                   NA                      NA
#> 133                    NA                   NA                      NA
#> 134                    NA                   NA                      NA
#> 135                    NA                   NA                      NA
#> 136                    NA                   NA                      NA
#> 137                    NA                   NA                      NA
#> 138                    NA                   NA                      NA
#> 139                    NA                   NA                      NA
#> 140                    NA                   NA                      NA
#> 141                    NA                   NA                      NA
#> 142                    NA                   NA                      NA
#> 143                    NA                   NA                      NA
#> 144                    NA                   NA                      NA
#> 145          6.362849e+12         6.362848e+12                 8481032
#> 146                    NA                   NA                      NA
#> 147                    NA                   NA                      NA
#> 148                    NA                   NA                      NA
#> 149                    NA                   NA                      NA
#> 150                    NA                   NA                      NA
#> 151                    NA                   NA                      NA
#> 152                    NA                   NA                      NA
#> 153                    NA                   NA                      NA
#> 154                    NA                   NA                      NA
#> 155                    NA                   NA                      NA
#> 156                    NA                   NA                      NA
#> 157                    NA                   NA                      NA
#> 158                    NA                   NA                      NA
#> 159                    NA                   NA                      NA
#> 160                    NA                   NA                      NA
#> 161                    NA                   NA                      NA
#> 162                    NA                   NA                      NA
#> 163                    NA                   NA                      NA
#> 164                    NA                   NA                      NA
#> 165                    NA                   NA                      NA
#> 166                    NA                   NA                      NA
#> 167                    NA                   NA                      NA
#> 168                    NA                   NA                      NA
#> 169                    NA                   NA                      NA
#> 170                    NA                   NA                      NA
#> 171                    NA                   NA                      NA
#> 172                    NA                   NA                      NA
#> 173                    NA                   NA                      NA
#> 174                    NA                   NA                      NA
#> 175                    NA                   NA                      NA
#> 176                    NA                   NA                      NA
#> 177                    NA                   NA                      NA
#> 178                    NA                   NA                      NA
#> 179                    NA                   NA                      NA
#> 180                    NA                   NA                      NA
#> 181                    NA                   NA                      NA
#> 182                    NA                   NA                      NA
#> 183                    NA                   NA                      NA
#> 184                    NA                   NA                      NA
#> 185                    NA                   NA                      NA
#> 186                    NA                   NA                      NA
#> 187                    NA                   NA                      NA
#> 188                    NA                   NA                      NA
#> 189                    NA                   NA                      NA
#> 190                    NA                   NA                      NA
#> 191                    NA                   NA                      NA
#> 192                    NA                   NA                      NA
#> 193                    NA                   NA                      NA
#> 194                    NA                   NA                      NA
#> 195                    NA                   NA                      NA
#> 196                    NA                   NA                      NA
#> 197                    NA                   NA                      NA
#> 198                    NA                   NA                      NA
#> 199                    NA                   NA                      NA
#> 200                    NA                   NA                      NA
#> 201                    NA                   NA                      NA
#> 202                    NA                   NA                      NA
#> 203                    NA                   NA                      NA
#> 204                    NA                   NA                      NA
#> 205                    NA                   NA                      NA
#> 206                    NA                   NA                      NA
#> 207                    NA                   NA                      NA
#> 208                    NA                   NA                      NA
#> 209                    NA                   NA                      NA
#> 210                    NA                   NA                      NA
#> 211                    NA                   NA                      NA
#> 212                    NA                   NA                      NA
#> 213                    NA                   NA                      NA
#> 214                    NA                   NA                      NA
#> 215                    NA                   NA                      NA
#> 216                    NA                   NA                      NA
#> 217                    NA                   NA                      NA
#> 218                    NA                   NA                      NA
#> 219                    NA                   NA                      NA
#> 220                    NA                   NA                      NA
#> 221                    NA                   NA                      NA
#> 222                    NA                   NA                      NA
#> 223                    NA                   NA                      NA
#> 224                    NA                   NA                      NA
#> 225                    NA                   NA                      NA
#> 226                    NA                   NA                      NA
#> 227                    NA                   NA                      NA
#> 228                    NA                   NA                      NA
#> 229                    NA                   NA                      NA
#> 230                    NA                   NA                      NA
#> 231                    NA                   NA                      NA
#> 232                    NA                   NA                      NA
#> 233                    NA                   NA                      NA
#> 234                    NA                   NA                      NA
#> 235                    NA                   NA                      NA
#> 236                    NA                   NA                      NA
#> 237                    NA                   NA                      NA
#> 238                    NA                   NA                      NA
#> 239                    NA                   NA                      NA
#> 240                    NA                   NA                      NA
#> 241                    NA                   NA                      NA
#> 242                    NA                   NA                      NA
#> 243                    NA                   NA                      NA
#> 244                    NA                   NA                      NA
#> 245                    NA                   NA                      NA
#> 246                    NA                   NA                      NA
#> 247                    NA                   NA                      NA
#> 248                    NA                   NA                      NA
#> 249                    NA                   NA                      NA
#> 250                    NA                   NA                      NA
#> 251                    NA                   NA                      NA
#> 252                    NA                   NA                      NA
#> 253                    NA                   NA                      NA
#> 254                    NA                   NA                      NA
#> 255                    NA                   NA                      NA
#> 256                    NA                   NA                      NA
#> 257                    NA                   NA                      NA
#> 258                    NA                   NA                      NA
#> 259                    NA                   NA                      NA
#> 260                    NA                   NA                      NA
#> 261                    NA                   NA                      NA
#> 262                    NA                   NA                      NA
#> 263                    NA                   NA                      NA
#> 264                    NA                   NA                      NA
#> 265                    NA                   NA                      NA
#> 266                    NA                   NA                      NA
#> 267                    NA                   NA                      NA
#> 268                    NA                   NA                      NA
#> 269                    NA                   NA                      NA
#> 270                    NA                   NA                      NA
#> 271                    NA                   NA                      NA
#> 272                    NA                   NA                      NA
#> 273                    NA                   NA                      NA
#> 274                    NA                   NA                      NA
#> 275                    NA                   NA                      NA
#> 276                    NA                   NA                      NA
#> 277                    NA                   NA                      NA
#> 278                    NA                   NA                      NA
#> 279                    NA                   NA                      NA
#> 280                    NA                   NA                      NA
#> 281                    NA                   NA                      NA
#> 282                    NA                   NA                      NA
#> 283                    NA                   NA                      NA
#> 284                    NA                   NA                      NA
#> 285                    NA                   NA                      NA
#> 286                    NA                   NA                      NA
#> 287                    NA                   NA                      NA
#> 288                    NA                   NA                      NA
#> 289                    NA                   NA                      NA
#> 290                    NA                   NA                      NA
#> 291                    NA                   NA                      NA
#> 292                    NA                   NA                      NA
#> 293                    NA                   NA                      NA
#> 294                    NA                   NA                      NA
#> 295                    NA                   NA                      NA
#> 296                    NA                   NA                      NA
#> 297          6.362852e+12         6.362853e+12                 8481524
#> 298                    NA                   NA                      NA
#> 299                    NA                   NA                      NA
#> 300                    NA                   NA                      NA
#> 301                    NA                   NA                      NA
#> 302                    NA                   NA                      NA
#> 303                    NA                   NA                      NA
#> 304                    NA                   NA                      NA
#> 305                    NA                   NA                      NA
#> 306                    NA                   NA                      NA
#> 307                    NA                   NA                      NA
#> 308                    NA                   NA                      NA
#> 309                    NA                   NA                      NA
#> 310                    NA                   NA                      NA
#> 311                    NA                   NA                      NA
#> 312                    NA                   NA                      NA
#> 313                    NA                   NA                      NA
#> 314                    NA                   NA                      NA
#> 315                    NA                   NA                      NA
#> 316                    NA                   NA                      NA
#> 317                    NA                   NA                      NA
#> 318                    NA                   NA                      NA
#> 319                    NA                   NA                      NA
#> 320                    NA                   NA                      NA
#> 321                    NA                   NA                      NA
#> 322                    NA                   NA                      NA
#> 323                    NA                   NA                      NA
#> 324                    NA                   NA                      NA
#> 325                    NA                   NA                      NA
#> 326                    NA                   NA                      NA
#> 327                    NA                   NA                      NA
#> 328                    NA                   NA                      NA
#> 329                    NA                   NA                      NA
#> 330                    NA                   NA                      NA
#> 331                    NA                   NA                      NA
#> 332                    NA                   NA                      NA
#> 333                    NA                   NA                      NA
#> 334                    NA                   NA                      NA
#> 335          6.362852e+12         6.362854e+12                      NA
#> 336                    NA                   NA                      NA
#> 337                    NA                   NA                      NA
#> 338                    NA                   NA                      NA
#> 339                    NA                   NA                      NA
#> 340                    NA                   NA                      NA
#> 341                    NA                   NA                      NA
#> 342                    NA                   NA                      NA
#> 343                    NA                   NA                      NA
#> 344                    NA                   NA                      NA
#> 345                    NA                   NA                      NA
#> 346                    NA                   NA                      NA
#> 347                    NA                   NA                      NA
#> 348                    NA                   NA                      NA
#> 349                    NA                   NA                      NA
#>     details.assist2PlayerTotal
#> 1                           NA
#> 2                           NA
#> 3                           NA
#> 4                           NA
#> 5                           NA
#> 6                           NA
#> 7                           NA
#> 8                           NA
#> 9                           NA
#> 10                          NA
#> 11                          NA
#> 12                          NA
#> 13                          NA
#> 14                          NA
#> 15                          NA
#> 16                          NA
#> 17                          NA
#> 18                          NA
#> 19                          NA
#> 20                          NA
#> 21                          NA
#> 22                          NA
#> 23                          NA
#> 24                          NA
#> 25                          NA
#> 26                          NA
#> 27                          NA
#> 28                          NA
#> 29                          NA
#> 30                          NA
#> 31                          NA
#> 32                          NA
#> 33                          NA
#> 34                          NA
#> 35                          NA
#> 36                          NA
#> 37                          NA
#> 38                          NA
#> 39                          NA
#> 40                          NA
#> 41                          NA
#> 42                          NA
#> 43                          NA
#> 44                          NA
#> 45                          NA
#> 46                          NA
#> 47                          NA
#> 48                          NA
#> 49                          NA
#> 50                          NA
#> 51                          NA
#> 52                          NA
#> 53                          NA
#> 54                          NA
#> 55                          NA
#> 56                          NA
#> 57                          NA
#> 58                          NA
#> 59                          NA
#> 60                          NA
#> 61                          NA
#> 62                          NA
#> 63                          NA
#> 64                          NA
#> 65                          NA
#> 66                          NA
#> 67                          NA
#> 68                          NA
#> 69                          NA
#> 70                          NA
#> 71                          NA
#> 72                          NA
#> 73                          NA
#> 74                          NA
#> 75                          NA
#> 76                          NA
#> 77                          NA
#> 78                          NA
#> 79                          NA
#> 80                          NA
#> 81                          NA
#> 82                          NA
#> 83                          NA
#> 84                          NA
#> 85                          NA
#> 86                          NA
#> 87                          NA
#> 88                          NA
#> 89                          NA
#> 90                          NA
#> 91                          NA
#> 92                          NA
#> 93                           1
#> 94                          NA
#> 95                          NA
#> 96                          NA
#> 97                          NA
#> 98                          NA
#> 99                          NA
#> 100                         NA
#> 101                         NA
#> 102                         NA
#> 103                         NA
#> 104                         NA
#> 105                         NA
#> 106                         NA
#> 107                         NA
#> 108                         NA
#> 109                         NA
#> 110                         NA
#> 111                         NA
#> 112                         NA
#> 113                         NA
#> 114                         NA
#> 115                         NA
#> 116                         NA
#> 117                         NA
#> 118                         NA
#> 119                         NA
#> 120                         NA
#> 121                         NA
#> 122                         NA
#> 123                         NA
#> 124                         NA
#> 125                         NA
#> 126                         NA
#> 127                         NA
#> 128                         NA
#> 129                         NA
#> 130                         NA
#> 131                         NA
#> 132                         NA
#> 133                         NA
#> 134                         NA
#> 135                         NA
#> 136                         NA
#> 137                         NA
#> 138                         NA
#> 139                         NA
#> 140                         NA
#> 141                         NA
#> 142                         NA
#> 143                         NA
#> 144                         NA
#> 145                          1
#> 146                         NA
#> 147                         NA
#> 148                         NA
#> 149                         NA
#> 150                         NA
#> 151                         NA
#> 152                         NA
#> 153                         NA
#> 154                         NA
#> 155                         NA
#> 156                         NA
#> 157                         NA
#> 158                         NA
#> 159                         NA
#> 160                         NA
#> 161                         NA
#> 162                         NA
#> 163                         NA
#> 164                         NA
#> 165                         NA
#> 166                         NA
#> 167                         NA
#> 168                         NA
#> 169                         NA
#> 170                         NA
#> 171                         NA
#> 172                         NA
#> 173                         NA
#> 174                         NA
#> 175                         NA
#> 176                         NA
#> 177                         NA
#> 178                         NA
#> 179                         NA
#> 180                         NA
#> 181                         NA
#> 182                         NA
#> 183                         NA
#> 184                         NA
#> 185                         NA
#> 186                         NA
#> 187                         NA
#> 188                         NA
#> 189                         NA
#> 190                         NA
#> 191                         NA
#> 192                         NA
#> 193                         NA
#> 194                         NA
#> 195                         NA
#> 196                         NA
#> 197                         NA
#> 198                         NA
#> 199                         NA
#> 200                         NA
#> 201                         NA
#> 202                         NA
#> 203                         NA
#> 204                         NA
#> 205                         NA
#> 206                         NA
#> 207                         NA
#> 208                         NA
#> 209                         NA
#> 210                         NA
#> 211                         NA
#> 212                         NA
#> 213                         NA
#> 214                         NA
#> 215                         NA
#> 216                         NA
#> 217                         NA
#> 218                         NA
#> 219                         NA
#> 220                         NA
#> 221                         NA
#> 222                         NA
#> 223                         NA
#> 224                         NA
#> 225                         NA
#> 226                         NA
#> 227                         NA
#> 228                         NA
#> 229                         NA
#> 230                         NA
#> 231                         NA
#> 232                         NA
#> 233                         NA
#> 234                         NA
#> 235                         NA
#> 236                         NA
#> 237                         NA
#> 238                         NA
#> 239                         NA
#> 240                         NA
#> 241                         NA
#> 242                         NA
#> 243                         NA
#> 244                         NA
#> 245                         NA
#> 246                         NA
#> 247                         NA
#> 248                         NA
#> 249                         NA
#> 250                         NA
#> 251                         NA
#> 252                         NA
#> 253                         NA
#> 254                         NA
#> 255                         NA
#> 256                         NA
#> 257                         NA
#> 258                         NA
#> 259                         NA
#> 260                         NA
#> 261                         NA
#> 262                         NA
#> 263                         NA
#> 264                         NA
#> 265                         NA
#> 266                         NA
#> 267                         NA
#> 268                         NA
#> 269                         NA
#> 270                         NA
#> 271                         NA
#> 272                         NA
#> 273                         NA
#> 274                         NA
#> 275                         NA
#> 276                         NA
#> 277                         NA
#> 278                         NA
#> 279                         NA
#> 280                         NA
#> 281                         NA
#> 282                         NA
#> 283                         NA
#> 284                         NA
#> 285                         NA
#> 286                         NA
#> 287                         NA
#> 288                         NA
#> 289                         NA
#> 290                         NA
#> 291                         NA
#> 292                         NA
#> 293                         NA
#> 294                         NA
#> 295                         NA
#> 296                         NA
#> 297                          1
#> 298                         NA
#> 299                         NA
#> 300                         NA
#> 301                         NA
#> 302                         NA
#> 303                         NA
#> 304                         NA
#> 305                         NA
#> 306                         NA
#> 307                         NA
#> 308                         NA
#> 309                         NA
#> 310                         NA
#> 311                         NA
#> 312                         NA
#> 313                         NA
#> 314                         NA
#> 315                         NA
#> 316                         NA
#> 317                         NA
#> 318                         NA
#> 319                         NA
#> 320                         NA
#> 321                         NA
#> 322                         NA
#> 323                         NA
#> 324                         NA
#> 325                         NA
#> 326                         NA
#> 327                         NA
#> 328                         NA
#> 329                         NA
#> 330                         NA
#> 331                         NA
#> 332                         NA
#> 333                         NA
#> 334                         NA
#> 335                         NA
#> 336                         NA
#> 337                         NA
#> 338                         NA
#> 339                         NA
#> 340                         NA
#> 341                         NA
#> 342                         NA
#> 343                         NA
#> 344                         NA
#> 345                         NA
#> 346                         NA
#> 347                         NA
#> 348                         NA
#> 349                         NA
#> 
#> $rosterSpots
#>    teamId playerId sweaterNumber positionCode
#> 1       1  8474593            25            G
#> 2       1  8474596            34            G
#> 3       1  8475193            90            L
#> 4       1  8475287            56            L
#> 5       1  8475455             5            D
#> 6       7  8475722            17            L
#> 7       1  8476292            18            L
#> 8       1  8476462             7            D
#> 9       1  8476474            11            R
#> 10      7  8477365            75            D
#> 11      1  8477508            42            C
#> 12      7  8477949            89            R
#> 13      7  8477979            96            R
#> 14      7  8478043            81            C
#> 15      1  8478399            71            D
#> 16      7  8478413            12            L
#> 17      1  8478414            28            R
#> 18      7  8479359            29            L
#> 19      1  8479407            63            L
#> 20      1  8479414            14            R
#> 21      7  8479420            72            C
#> 22      1  8480002            13            C
#> 23      7  8480035            10            D
#> 24      7  8480045             1            G
#> 25      1  8480192             8            D
#> 26      7  8480802            71            C
#> 27      7  8480807            23            D
#> 28      7  8480839            26            D
#> 29      1  8481032            47            C
#> 30      7  8481524             4            D
#> 31      7  8481528            24            C
#> 32      1  8481559            86            C
#> 33      7  8482097            22            R
#> 34      1  8482110            91            C
#> 35      7  8482175            77            R
#> 36      7  8482221            27            G
#> 37      7  8482671            25            D
#> 38      1  8483429            24            D
#> 39      1  8483495            17            D
#> 40      7  8484145             9            L
#>                                                     headshot firstName.default
#> 1  https://assets.nhle.com/mugs/nhl/20242025/NJD/8474593.png             Jacob
#> 2  https://assets.nhle.com/mugs/nhl/20242025/NJD/8474596.png              Jake
#> 3  https://assets.nhle.com/mugs/nhl/20242025/NJD/8475193.png             Tomas
#> 4  https://assets.nhle.com/mugs/nhl/20242025/NJD/8475287.png              Erik
#> 5  https://assets.nhle.com/mugs/nhl/20242025/NJD/8475455.png           Brenden
#> 6  https://assets.nhle.com/mugs/nhl/20242025/BUF/8475722.png             Jason
#> 7  https://assets.nhle.com/mugs/nhl/20242025/NJD/8476292.png            Ondrej
#> 8  https://assets.nhle.com/mugs/nhl/20242025/NJD/8476462.png            Dougie
#> 9  https://assets.nhle.com/mugs/nhl/20242025/NJD/8476474.png            Stefan
#> 10 https://assets.nhle.com/mugs/nhl/20242025/BUF/8477365.png            Connor
#> 11 https://assets.nhle.com/mugs/nhl/20242025/NJD/8477508.png            Curtis
#> 12 https://assets.nhle.com/mugs/nhl/20242025/BUF/8477949.png              Alex
#> 13 https://assets.nhle.com/mugs/nhl/20242025/BUF/8477979.png           Nicolas
#> 14 https://assets.nhle.com/mugs/nhl/20242025/BUF/8478043.png               Sam
#> 15 https://assets.nhle.com/mugs/nhl/20242025/NJD/8478399.png             Jonas
#> 16 https://assets.nhle.com/mugs/nhl/20242025/BUF/8478413.png            Jordan
#> 17 https://assets.nhle.com/mugs/nhl/20242025/NJD/8478414.png              Timo
#> 18 https://assets.nhle.com/mugs/nhl/20242025/BUF/8479359.png              Beck
#> 19 https://assets.nhle.com/mugs/nhl/20242025/NJD/8479407.png            Jesper
#> 20 https://assets.nhle.com/mugs/nhl/20242025/NJD/8479414.png            Nathan
#> 21 https://assets.nhle.com/mugs/nhl/20242025/BUF/8479420.png              Tage
#> 22 https://assets.nhle.com/mugs/nhl/20242025/NJD/8480002.png              Nico
#> 23 https://assets.nhle.com/mugs/nhl/20242025/BUF/8480035.png             Henri
#> 24 https://assets.nhle.com/mugs/nhl/20242025/BUF/8480045.png        Ukko-Pekka
#> 25 https://assets.nhle.com/mugs/nhl/20242025/NJD/8480192.png         Johnathan
#> 26 https://assets.nhle.com/mugs/nhl/20242025/BUF/8480802.png              Ryan
#> 27 https://assets.nhle.com/mugs/nhl/20242025/BUF/8480807.png           Mattias
#> 28 https://assets.nhle.com/mugs/nhl/20242025/BUF/8480839.png            Rasmus
#> 29 https://assets.nhle.com/mugs/nhl/20242025/NJD/8481032.png              Paul
#> 30 https://assets.nhle.com/mugs/nhl/20242025/BUF/8481524.png             Bowen
#> 31 https://assets.nhle.com/mugs/nhl/20242025/BUF/8481528.png             Dylan
#> 32 https://assets.nhle.com/mugs/nhl/20242025/NJD/8481559.png              Jack
#> 33 https://assets.nhle.com/mugs/nhl/20242025/BUF/8482097.png              Jack
#> 34 https://assets.nhle.com/mugs/nhl/20242025/NJD/8482110.png            Dawson
#> 35 https://assets.nhle.com/mugs/nhl/20242025/BUF/8482175.png                JJ
#> 36 https://assets.nhle.com/mugs/nhl/20242025/BUF/8482221.png             Devon
#> 37 https://assets.nhle.com/mugs/nhl/20242025/BUF/8482671.png              Owen
#> 38 https://assets.nhle.com/mugs/nhl/20242025/NJD/8483429.png            Seamus
#> 39 https://assets.nhle.com/mugs/nhl/20242025/NJD/8483495.png             Simon
#> 40 https://assets.nhle.com/mugs/nhl/20242025/BUF/8484145.png              Zach
#>    firstName.cs firstName.sk lastName.default lastName.cs lastName.fi
#> 1          <NA>         <NA>        Markstrom   Markström   Markström
#> 2          <NA>         <NA>            Allen        <NA>        <NA>
#> 3         Tomáš        Tomáš            Tatar        <NA>        <NA>
#> 4          <NA>         <NA>            Haula        <NA>        <NA>
#> 5          <NA>         <NA>           Dillon        <NA>        <NA>
#> 6          <NA>         <NA>           Zucker        <NA>        <NA>
#> 7        Ondřej       Ondřej            Palat       Palát        <NA>
#> 8          <NA>         <NA>         Hamilton        <NA>        <NA>
#> 9          <NA>         <NA>           Noesen        <NA>        <NA>
#> 10         <NA>         <NA>          Clifton        <NA>        <NA>
#> 11         <NA>         <NA>            Lazar        <NA>        <NA>
#> 12         <NA>         <NA>             Tuch        <NA>        <NA>
#> 13         <NA>         <NA>       Aube-Kubel        <NA>        <NA>
#> 14         <NA>         <NA>         Lafferty        <NA>        <NA>
#> 15         <NA>         <NA>     Siegenthaler        <NA>        <NA>
#> 16         <NA>         <NA>         Greenway        <NA>        <NA>
#> 17         <NA>         <NA>            Meier        <NA>        <NA>
#> 18         <NA>         <NA>        Malenstyn        <NA>        <NA>
#> 19         <NA>         <NA>            Bratt        <NA>        <NA>
#> 20         <NA>         <NA>          Bastian        <NA>        <NA>
#> 21         <NA>         <NA>         Thompson        <NA>        <NA>
#> 22         <NA>         <NA>         Hischier        <NA>        <NA>
#> 23         <NA>         <NA>        Jokiharju        <NA>        <NA>
#> 24         <NA>         <NA>        Luukkonen        <NA>        <NA>
#> 25         <NA>         <NA>        Kovacevic        <NA>        <NA>
#> 26         <NA>         <NA>           McLeod        <NA>        <NA>
#> 27         <NA>         <NA>       Samuelsson        <NA>        <NA>
#> 28         <NA>         <NA>           Dahlin        <NA>        <NA>
#> 29         <NA>         <NA>           Cotter        <NA>        <NA>
#> 30         <NA>         <NA>            Byram        <NA>        <NA>
#> 31         <NA>         <NA>           Cozens        <NA>        <NA>
#> 32         <NA>         <NA>           Hughes        <NA>        <NA>
#> 33         <NA>         <NA>            Quinn        <NA>        <NA>
#> 34         <NA>         <NA>           Mercer        <NA>        <NA>
#> 35         <NA>         <NA>          Peterka        <NA>        <NA>
#> 36         <NA>         <NA>             Levi        <NA>        <NA>
#> 37         <NA>         <NA>            Power        <NA>        <NA>
#> 38         <NA>         <NA>            Casey        <NA>        <NA>
#> 39        Šimon        Šimon            Nemec        <NA>        <NA>
#> 40         <NA>         <NA>           Benson        <NA>        <NA>
#>    lastName.sk lastName.sv
#> 1    Markström   Markström
#> 2         <NA>        <NA>
#> 3         <NA>        <NA>
#> 4         <NA>        <NA>
#> 5         <NA>        <NA>
#> 6         <NA>        <NA>
#> 7        Palát        <NA>
#> 8         <NA>        <NA>
#> 9         <NA>        <NA>
#> 10        <NA>        <NA>
#> 11        <NA>        <NA>
#> 12        <NA>        <NA>
#> 13        <NA>        <NA>
#> 14        <NA>        <NA>
#> 15        <NA>        <NA>
#> 16        <NA>        <NA>
#> 17        <NA>        <NA>
#> 18        <NA>        <NA>
#> 19        <NA>        <NA>
#> 20        <NA>        <NA>
#> 21        <NA>        <NA>
#> 22        <NA>        <NA>
#> 23        <NA>        <NA>
#> 24        <NA>        <NA>
#> 25        <NA>        <NA>
#> 26        <NA>        <NA>
#> 27        <NA>        <NA>
#> 28        <NA>        <NA>
#> 29        <NA>        <NA>
#> 30        <NA>        <NA>
#> 31        <NA>        <NA>
#> 32        <NA>        <NA>
#> 33        <NA>        <NA>
#> 34        <NA>        <NA>
#> 35        <NA>        <NA>
#> 36        <NA>        <NA>
#> 37        <NA>        <NA>
#> 38        <NA>        <NA>
#> 39        <NA>        <NA>
#> 40        <NA>        <NA>
#> 
#> $regPeriods
#> [1] 3
#> 
#> $summary
#> named list()
#> 
# }
```
