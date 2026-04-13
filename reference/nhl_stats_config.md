# **NHL Stats API — Config**

Returns the Stats REST API configuration payload
(`https://api.nhle.com/stats/rest/{lang}/config`). This endpoint is
non-tabular: the raw parsed list is returned as-is rather than wrapped
in a `fastRhockey_data` tibble.

## Usage

``` r
nhl_stats_config(lang = "en")
```

## Arguments

- lang:

  Character language code. Default `"en"`.

## Value

A parsed list of configuration values, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_config())
#> $playerReportData
#> $playerReportData$summary
#> $playerReportData$summary$game
#> $playerReportData$summary$game$displayItems
#>  [1] "playerId"           "skaterFullName"     "gameId"            
#>  [4] "opponentTeamAbbrev" "gameDate"           "teamAbbrev"        
#>  [7] "shootsCatches"      "positionCode"       "gamesPlayed"       
#> [10] "goals"              "assists"            "points"            
#> [13] "plusMinus"          "penaltyMinutes"     "pointsPerGame"     
#> [16] "evGoals"            "evPoints"           "ppGoals"           
#> [19] "ppPoints"           "shGoals"            "shPoints"          
#> [22] "otGoals"            "gameWinningGoals"   "shots"             
#> [25] "shootingPct"        "timeOnIcePerGame"   "faceoffWinPct"     
#> 
#> $playerReportData$summary$game$resultFilters
#>  [1] "gamesPlayed"      "goals"            "assists"          "points"          
#>  [5] "plusMinus"        "penaltyMinutes"   "pointsPerGame"    "evGoals"         
#>  [9] "evPoints"         "ppGoals"          "ppPoints"         "shGoals"         
#> [13] "shPoints"         "otGoals"          "gameWinningGoals" "shots"           
#> [17] "shootingPct"      "timeOnIcePerGame" "faceoffWinPct"   
#> 
#> $playerReportData$summary$game$sortKeys
#> [1] "points"  "goals"   "assists"
#> 
#> 
#> $playerReportData$summary$season
#> $playerReportData$summary$season$displayItems
#>  [1] "playerId"         "skaterFullName"   "seasonId"         "teamAbbrevs"     
#>  [5] "shootsCatches"    "positionCode"     "gamesPlayed"      "goals"           
#>  [9] "assists"          "points"           "plusMinus"        "penaltyMinutes"  
#> [13] "pointsPerGame"    "evGoals"          "evPoints"         "ppGoals"         
#> [17] "ppPoints"         "shGoals"          "shPoints"         "otGoals"         
#> [21] "gameWinningGoals" "shots"            "shootingPct"      "timeOnIcePerGame"
#> [25] "faceoffWinPct"   
#> 
#> $playerReportData$summary$season$resultFilters
#>  [1] "gamesPlayed"      "goals"            "assists"          "points"          
#>  [5] "plusMinus"        "penaltyMinutes"   "pointsPerGame"    "evGoals"         
#>  [9] "evPoints"         "ppGoals"          "ppPoints"         "shGoals"         
#> [13] "shPoints"         "otGoals"          "gameWinningGoals" "shots"           
#> [17] "shootingPct"      "timeOnIcePerGame" "faceoffWinPct"   
#> 
#> $playerReportData$summary$season$sortKeys
#> [1] "points"  "goals"   "assists"
#> 
#> 
#> 
#> $playerReportData$realtime
#> $playerReportData$realtime$game
#> $playerReportData$realtime$game$displayItems
#>  [1] "playerId"                    "skaterFullName"             
#>  [3] "gameId"                      "opponentTeamAbbrev"         
#>  [5] "gameDate"                    "teamAbbrev"                 
#>  [7] "shootsCatches"               "positionCode"               
#>  [9] "gamesPlayed"                 "timeOnIcePerGame"           
#> [11] "hits"                        "hitsPer60"                  
#> [13] "blockedShots"                "blockedShotsPer60"          
#> [15] "giveaways"                   "giveawaysPer60"             
#> [17] "takeaways"                   "takeawaysPer60"             
#> [19] "firstGoals"                  "otGoals"                    
#> [21] "emptyNetGoals"               "emptyNetAssists"            
#> [23] "emptyNetPoints"              "totalShotAttempts"          
#> [25] "shotAttemptsBlocked"         "missedShots"                
#> [27] "missedShotWideOfNet"         "missedShotOverNet"          
#> [29] "missedShotGoalpost"          "missedShotCrossbar"         
#> [31] "missedShotShort"             "missedShotFailedBankAttempt"
#> 
#> $playerReportData$realtime$game$resultFilters
#>  [1] "gamesPlayed"                 "timeOnIcePerGame"           
#>  [3] "hits"                        "hitsPer60"                  
#>  [5] "blockedShots"                "blockedShotsPer60"          
#>  [7] "giveaways"                   "giveawaysPer60"             
#>  [9] "takeaways"                   "takeawaysPer60"             
#> [11] "firstGoals"                  "otGoals"                    
#> [13] "emptyNetGoals"               "emptyNetAssists"            
#> [15] "emptyNetPoints"              "totalShotAttempts"          
#> [17] "shotAttemptsBlocked"         "missedShots"                
#> [19] "missedShotWideOfNet"         "missedShotOverNet"          
#> [21] "missedShotGoalpost"          "missedShotCrossbar"         
#> [23] "missedShotShort"             "missedShotFailedBankAttempt"
#> 
#> $playerReportData$realtime$game$sortKeys
#> [1] "hits"
#> 
#> 
#> $playerReportData$realtime$season
#> $playerReportData$realtime$season$displayItems
#>  [1] "playerId"                    "skaterFullName"             
#>  [3] "seasonId"                    "teamAbbrevs"                
#>  [5] "shootsCatches"               "positionCode"               
#>  [7] "gamesPlayed"                 "timeOnIcePerGame"           
#>  [9] "hits"                        "hitsPer60"                  
#> [11] "blockedShots"                "blockedShotsPer60"          
#> [13] "giveaways"                   "giveawaysPer60"             
#> [15] "takeaways"                   "takeawaysPer60"             
#> [17] "firstGoals"                  "otGoals"                    
#> [19] "emptyNetGoals"               "emptyNetAssists"            
#> [21] "emptyNetPoints"              "totalShotAttempts"          
#> [23] "shotAttemptsBlocked"         "missedShots"                
#> [25] "missedShotWideOfNet"         "missedShotOverNet"          
#> [27] "missedShotGoalpost"          "missedShotCrossbar"         
#> [29] "missedShotShort"             "missedShotFailedBankAttempt"
#> 
#> $playerReportData$realtime$season$resultFilters
#>  [1] "gamesPlayed"                 "timeOnIcePerGame"           
#>  [3] "hits"                        "hitsPer60"                  
#>  [5] "blockedShots"                "blockedShotsPer60"          
#>  [7] "giveaways"                   "giveawaysPer60"             
#>  [9] "takeaways"                   "takeawaysPer60"             
#> [11] "firstGoals"                  "otGoals"                    
#> [13] "emptyNetGoals"               "emptyNetAssists"            
#> [15] "emptyNetPoints"              "totalShotAttempts"          
#> [17] "shotAttemptsBlocked"         "missedShots"                
#> [19] "missedShotWideOfNet"         "missedShotOverNet"          
#> [21] "missedShotGoalpost"          "missedShotCrossbar"         
#> [23] "missedShotShort"             "missedShotFailedBankAttempt"
#> 
#> $playerReportData$realtime$season$sortKeys
#> [1] "hits"
#> 
#> 
#> 
#> $playerReportData$penalties
#> $playerReportData$penalties$game
#> $playerReportData$penalties$game$displayItems
#>  [1] "playerId"                   "skaterFullName"            
#>  [3] "gameId"                     "opponentTeamAbbrev"        
#>  [5] "gameDate"                   "teamAbbrev"                
#>  [7] "positionCode"               "gamesPlayed"               
#>  [9] "goals"                      "assists"                   
#> [11] "points"                     "penaltyMinutes"            
#> [13] "penaltySecondsPerGame"      "timeOnIcePerGame"          
#> [15] "penaltyMinutesPerTimeOnIce" "penaltiesDrawn"            
#> [17] "penalties"                  "netPenalties"              
#> [19] "penaltiesDrawnPer60"        "penaltiesTakenPer60"       
#> [21] "netPenaltiesPer60"          "minorPenalties"            
#> [23] "majorPenalties"             "matchPenalties"            
#> [25] "misconductPenalties"        "gameMisconductPenalties"   
#> 
#> $playerReportData$penalties$game$resultFilters
#>  [1] "gamesPlayed"                "goals"                     
#>  [3] "assists"                    "points"                    
#>  [5] "penaltyMinutes"             "penaltySecondsPerGame"     
#>  [7] "timeOnIcePerGame"           "penaltyMinutesPerTimeOnIce"
#>  [9] "penaltiesDrawn"             "penalties"                 
#> [11] "netPenalties"               "penaltiesDrawnPer60"       
#> [13] "penaltiesTakenPer60"        "netPenaltiesPer60"         
#> [15] "minorPenalties"             "majorPenalties"            
#> [17] "matchPenalties"             "misconductPenalties"       
#> [19] "gameMisconductPenalties"   
#> 
#> $playerReportData$penalties$game$sortKeys
#> [1] "penaltyMinutes"
#> 
#> 
#> $playerReportData$penalties$season
#> $playerReportData$penalties$season$displayItems
#>  [1] "playerId"                   "skaterFullName"            
#>  [3] "seasonId"                   "teamAbbrevs"               
#>  [5] "positionCode"               "gamesPlayed"               
#>  [7] "goals"                      "assists"                   
#>  [9] "points"                     "penaltyMinutes"            
#> [11] "penaltySecondsPerGame"      "timeOnIcePerGame"          
#> [13] "penaltyMinutesPerTimeOnIce" "penaltiesDrawn"            
#> [15] "penalties"                  "netPenalties"              
#> [17] "penaltiesDrawnPer60"        "penaltiesTakenPer60"       
#> [19] "netPenaltiesPer60"          "minorPenalties"            
#> [21] "majorPenalties"             "matchPenalties"            
#> [23] "misconductPenalties"        "gameMisconductPenalties"   
#> 
#> $playerReportData$penalties$season$resultFilters
#>  [1] "gamesPlayed"                "goals"                     
#>  [3] "assists"                    "points"                    
#>  [5] "penaltyMinutes"             "penaltySecondsPerGame"     
#>  [7] "timeOnIcePerGame"           "penaltyMinutesPerTimeOnIce"
#>  [9] "penaltiesDrawn"             "penalties"                 
#> [11] "netPenalties"               "penaltiesDrawnPer60"       
#> [13] "penaltiesTakenPer60"        "netPenaltiesPer60"         
#> [15] "minorPenalties"             "majorPenalties"            
#> [17] "matchPenalties"             "misconductPenalties"       
#> [19] "gameMisconductPenalties"   
#> 
#> $playerReportData$penalties$season$sortKeys
#> [1] "penaltyMinutes"
#> 
#> 
#> 
#> $playerReportData$shootout
#> $playerReportData$shootout$game
#> $playerReportData$shootout$game$displayItems
#>  [1] "playerId"                        "skaterFullName"                 
#>  [3] "gameId"                          "opponentTeamAbbrev"             
#>  [5] "gameDate"                        "teamAbbrev"                     
#>  [7] "shootsCatches"                   "positionCode"                   
#>  [9] "shootoutGamesPlayed"             "shootoutGoals"                  
#> [11] "shootoutShots"                   "shootoutShootingPct"            
#> [13] "shootoutGameDecidingGoals"       "careerShootoutGamesPlayed"      
#> [15] "careerShootoutGoals"             "careerShootoutShots"            
#> [17] "careerShootoutShootingPct"       "careerShootoutGameDecidingGoals"
#> 
#> $playerReportData$shootout$game$resultFilters
#>  [1] "shootoutGamesPlayed"             "shootoutGoals"                  
#>  [3] "shootoutShots"                   "shootoutShootingPct"            
#>  [5] "shootoutGameDecidingGoals"       "careerShootoutGamesPlayed"      
#>  [7] "careerShootoutGoals"             "careerShootoutShots"            
#>  [9] "careerShootoutShootingPct"       "careerShootoutGameDecidingGoals"
#> 
#> $playerReportData$shootout$game$sortKeys
#> [1] "shootoutGoals"
#> 
#> 
#> $playerReportData$shootout$season
#> $playerReportData$shootout$season$displayItems
#>  [1] "playerId"                        "skaterFullName"                 
#>  [3] "seasonId"                        "teamAbbrevs"                    
#>  [5] "shootsCatches"                   "positionCode"                   
#>  [7] "shootoutGamesPlayed"             "shootoutGoals"                  
#>  [9] "shootoutShots"                   "shootoutShootingPct"            
#> [11] "shootoutGameDecidingGoals"       "careerShootoutGamesPlayed"      
#> [13] "careerShootoutGoals"             "careerShootoutShots"            
#> [15] "careerShootoutShootingPct"       "careerShootoutGameDecidingGoals"
#> 
#> $playerReportData$shootout$season$resultFilters
#>  [1] "shootoutGamesPlayed"             "shootoutGoals"                  
#>  [3] "shootoutShots"                   "shootoutShootingPct"            
#>  [5] "shootoutGameDecidingGoals"       "careerShootoutGamesPlayed"      
#>  [7] "careerShootoutGoals"             "careerShootoutShots"            
#>  [9] "careerShootoutShootingPct"       "careerShootoutGameDecidingGoals"
#> 
#> $playerReportData$shootout$season$sortKeys
#> [1] "shootoutGoals"
#> 
#> 
#> 
#> $playerReportData$bios
#> $playerReportData$bios$game
#> $playerReportData$bios$game$displayItems
#>  [1] "playerId"               "skaterFullName"         "currentTeamAbbrev"     
#>  [4] "shootsCatches"          "positionCode"           "birthDate"             
#>  [7] "birthCity"              "birthStateProvinceCode" "birthCountryCode"      
#> [10] "nationalityCode"        "height"                 "weight"                
#> [13] "draftYear"              "draftRound"             "draftOverall"          
#> [16] "firstSeasonForGameType" "isInHallOfFameYn"       "gamesPlayed"           
#> [19] "goals"                  "assists"                "points"                
#> 
#> $playerReportData$bios$game$resultFilters
#> [1] "height"       "weight"       "draftYear"    "draftRound"   "draftOverall"
#> [6] "gamesPlayed"  "goals"        "assists"      "points"      
#> 
#> $playerReportData$bios$game$sortKeys
#> [1] "skaterFullName"
#> 
#> 
#> $playerReportData$bios$season
#> $playerReportData$bios$season$displayItems
#>  [1] "playerId"               "skaterFullName"         "currentTeamAbbrev"     
#>  [4] "shootsCatches"          "positionCode"           "birthDate"             
#>  [7] "birthCity"              "birthStateProvinceCode" "birthCountryCode"      
#> [10] "nationalityCode"        "height"                 "weight"                
#> [13] "draftYear"              "draftRound"             "draftOverall"          
#> [16] "firstSeasonForGameType" "isInHallOfFameYn"       "gamesPlayed"           
#> [19] "goals"                  "assists"                "points"                
#> 
#> $playerReportData$bios$season$resultFilters
#> [1] "height"       "weight"       "draftYear"    "draftRound"   "draftOverall"
#> [6] "gamesPlayed"  "goals"        "assists"      "points"      
#> 
#> $playerReportData$bios$season$sortKeys
#> [1] "skaterFullName"
#> 
#> 
#> 
#> $playerReportData$shottype
#> $playerReportData$shottype$game
#> $playerReportData$shottype$game$displayItems
#>  [1] "playerId"               "skaterFullName"         "gameId"                
#>  [4] "opponentTeamAbbrev"     "gameDate"               "teamAbbrev"            
#>  [7] "gamesPlayed"            "goals"                  "goalsWrist"            
#> [10] "goalsSnap"              "goalsSlap"              "goalsBackhand"         
#> [13] "goalsTipIn"             "goalsDeflected"         "goalsWrapAround"       
#> [16] "goalsPoke"              "goalsCradle"            "goalsBetweenLegs"      
#> [19] "goalsBat"               "shotsOnNetWrist"        "shotsOnNetSnap"        
#> [22] "shotsOnNetSlap"         "shotsOnNetBackhand"     "shotsOnNetTipIn"       
#> [25] "shotsOnNetDeflected"    "shotsOnNetWrapAround"   "shotsOnNetPoke"        
#> [28] "shotsOnNetCradle"       "shotsOnNetBetweenLegs"  "shotsOnNetBat"         
#> [31] "shootingPct"            "shootingPctWrist"       "shootingPctSnap"       
#> [34] "shootingPctSlap"        "shootingPctBackhand"    "shootingPctTipIn"      
#> [37] "shootingPctDeflected"   "shootingPctWrapAround"  "shootingPctPoke"       
#> [40] "shootingPctCradle"      "shootingPctBetweenLegs" "shootingPctBat"        
#> 
#> $playerReportData$shottype$game$resultFilters
#>  [1] "gamesPlayed"            "goals"                  "goalsWrist"            
#>  [4] "goalsSnap"              "goalsSlap"              "goalsBackhand"         
#>  [7] "goalsTipIn"             "goalsDeflected"         "goalsWrapAround"       
#> [10] "goalsPoke"              "goalsCradle"            "goalsBetweenLegs"      
#> [13] "goalsBat"               "shotsOnNetWrist"        "shotsOnNetSnap"        
#> [16] "shotsOnNetSlap"         "shotsOnNetBackhand"     "shotsOnNetTipIn"       
#> [19] "shotsOnNetDeflected"    "shotsOnNetWrapAround"   "shotsOnNetPoke"        
#> [22] "shotsOnNetCradle"       "shotsOnNetBetweenLegs"  "shotsOnNetBat"         
#> [25] "shootingPct"            "shootingPctWrist"       "shootingPctSnap"       
#> [28] "shootingPctSlap"        "shootingPctBackhand"    "shootingPctTipIn"      
#> [31] "shootingPctDeflected"   "shootingPctWrapAround"  "shootingPctPoke"       
#> [34] "shootingPctCradle"      "shootingPctBetweenLegs" "shootingPctBat"        
#> 
#> $playerReportData$shottype$game$sortKeys
#> [1] "shootingPct"    "shootingPctBat"
#> 
#> 
#> $playerReportData$shottype$season
#> $playerReportData$shottype$season$displayItems
#>  [1] "playerId"               "skaterFullName"         "seasonId"              
#>  [4] "teamAbbrevs"            "gamesPlayed"            "goals"                 
#>  [7] "goalsWrist"             "goalsSnap"              "goalsSlap"             
#> [10] "goalsBackhand"          "goalsTipIn"             "goalsDeflected"        
#> [13] "goalsWrapAround"        "goalsPoke"              "goalsCradle"           
#> [16] "goalsBetweenLegs"       "goalsBat"               "shotsOnNetWrist"       
#> [19] "shotsOnNetSnap"         "shotsOnNetSlap"         "shotsOnNetBackhand"    
#> [22] "shotsOnNetTipIn"        "shotsOnNetDeflected"    "shotsOnNetWrapAround"  
#> [25] "shotsOnNetPoke"         "shotsOnNetCradle"       "shotsOnNetBetweenLegs" 
#> [28] "shotsOnNetBat"          "shootingPct"            "shootingPctWrist"      
#> [31] "shootingPctSnap"        "shootingPctSlap"        "shootingPctBackhand"   
#> [34] "shootingPctTipIn"       "shootingPctDeflected"   "shootingPctWrapAround" 
#> [37] "shootingPctPoke"        "shootingPctCradle"      "shootingPctBetweenLegs"
#> [40] "shootingPctBat"        
#> 
#> $playerReportData$shottype$season$resultFilters
#>  [1] "gamesPlayed"            "goals"                  "goalsWrist"            
#>  [4] "goalsSnap"              "goalsSlap"              "goalsBackhand"         
#>  [7] "goalsTipIn"             "goalsDeflected"         "goalsWrapAround"       
#> [10] "goalsPoke"              "goalsCradle"            "goalsBetweenLegs"      
#> [13] "goalsBat"               "shotsOnNetWrist"        "shotsOnNetSnap"        
#> [16] "shotsOnNetSlap"         "shotsOnNetBackhand"     "shotsOnNetTipIn"       
#> [19] "shotsOnNetDeflected"    "shotsOnNetWrapAround"   "shotsOnNetPoke"        
#> [22] "shotsOnNetCradle"       "shotsOnNetBetweenLegs"  "shotsOnNetBat"         
#> [25] "shootingPct"            "shootingPctWrist"       "shootingPctSnap"       
#> [28] "shootingPctSlap"        "shootingPctBackhand"    "shootingPctTipIn"      
#> [31] "shootingPctDeflected"   "shootingPctWrapAround"  "shootingPctPoke"       
#> [34] "shootingPctCradle"      "shootingPctBetweenLegs" "shootingPctBat"        
#> 
#> $playerReportData$shottype$season$sortKeys
#> [1] "shootingPct"    "shootingPctBat"
#> 
#> 
#> 
#> $playerReportData$faceoffpercentages
#> $playerReportData$faceoffpercentages$game
#> $playerReportData$faceoffpercentages$game$displayItems
#>  [1] "playerId"                "skaterFullName"         
#>  [3] "gameId"                  "opponentTeamAbbrev"     
#>  [5] "gameDate"                "teamAbbrev"             
#>  [7] "shootsCatches"           "positionCode"           
#>  [9] "gamesPlayed"             "timeOnIcePerGame"       
#> [11] "totalFaceoffs"           "evFaceoffs"             
#> [13] "ppFaceoffs"              "shFaceoffs"             
#> [15] "offensiveZoneFaceoffs"   "neutralZoneFaceoffs"    
#> [17] "defensiveZoneFaceoffs"   "faceoffWinPct"          
#> [19] "evFaceoffPct"            "ppFaceoffPct"           
#> [21] "shFaceoffPct"            "offensiveZoneFaceoffPct"
#> [23] "neutralZoneFaceoffPct"   "defensiveZoneFaceoffPct"
#> 
#> $playerReportData$faceoffpercentages$game$resultFilters
#>  [1] "gamesPlayed"             "timeOnIcePerGame"       
#>  [3] "totalFaceoffs"           "evFaceoffs"             
#>  [5] "ppFaceoffs"              "shFaceoffs"             
#>  [7] "offensiveZoneFaceoffs"   "neutralZoneFaceoffs"    
#>  [9] "defensiveZoneFaceoffs"   "faceoffWinPct"          
#> [11] "evFaceoffPct"            "ppFaceoffPct"           
#> [13] "shFaceoffPct"            "offensiveZoneFaceoffPct"
#> [15] "neutralZoneFaceoffPct"   "defensiveZoneFaceoffPct"
#> 
#> $playerReportData$faceoffpercentages$game$sortKeys
#> [1] "totalFaceoffs"
#> 
#> 
#> $playerReportData$faceoffpercentages$season
#> $playerReportData$faceoffpercentages$season$displayItems
#>  [1] "playerId"                "skaterFullName"         
#>  [3] "seasonId"                "teamAbbrevs"            
#>  [5] "shootsCatches"           "positionCode"           
#>  [7] "gamesPlayed"             "timeOnIcePerGame"       
#>  [9] "totalFaceoffs"           "evFaceoffs"             
#> [11] "ppFaceoffs"              "shFaceoffs"             
#> [13] "offensiveZoneFaceoffs"   "neutralZoneFaceoffs"    
#> [15] "defensiveZoneFaceoffs"   "faceoffWinPct"          
#> [17] "evFaceoffPct"            "ppFaceoffPct"           
#> [19] "shFaceoffPct"            "offensiveZoneFaceoffPct"
#> [21] "neutralZoneFaceoffPct"   "defensiveZoneFaceoffPct"
#> 
#> $playerReportData$faceoffpercentages$season$resultFilters
#>  [1] "gamesPlayed"             "timeOnIcePerGame"       
#>  [3] "totalFaceoffs"           "evFaceoffs"             
#>  [5] "ppFaceoffs"              "shFaceoffs"             
#>  [7] "offensiveZoneFaceoffs"   "neutralZoneFaceoffs"    
#>  [9] "defensiveZoneFaceoffs"   "faceoffWinPct"          
#> [11] "evFaceoffPct"            "ppFaceoffPct"           
#> [13] "shFaceoffPct"            "offensiveZoneFaceoffPct"
#> [15] "neutralZoneFaceoffPct"   "defensiveZoneFaceoffPct"
#> 
#> $playerReportData$faceoffpercentages$season$sortKeys
#> [1] "totalFaceoffs"
#> 
#> 
#> 
#> $playerReportData$percentages
#> $playerReportData$percentages$game
#> $playerReportData$percentages$game$displayItems
#>  [1] "playerId"                     "skaterFullName"              
#>  [3] "gameId"                       "opponentTeamAbbrev"          
#>  [5] "gameDate"                     "teamAbbrev"                  
#>  [7] "shootsCatches"                "positionCode"                
#>  [9] "gamesPlayed"                  "timeOnIcePerGame5v5"         
#> [11] "satPercentage"                "satPercentageAhead"          
#> [13] "satPercentageTied"            "satPercentageBehind"         
#> [15] "satPercentageClose"           "satRelative"                 
#> [17] "usatPercentage"               "usatPercentageAhead"         
#> [19] "usatPercentageTied"           "usatPercentageBehind"        
#> [21] "usatPrecentageClose"          "usatRelative"                
#> [23] "zoneStartPct5v5"              "shootingPct5v5"              
#> [25] "skaterSavePct5v5"             "skaterShootingPlusSavePct5v5"
#> 
#> $playerReportData$percentages$game$resultFilters
#>  [1] "gamesPlayed"                  "timeOnIcePerGame5v5"         
#>  [3] "satPercentage"                "satPercentageAhead"          
#>  [5] "satPercentageTied"            "satPercentageBehind"         
#>  [7] "satPercentageClose"           "satRelative"                 
#>  [9] "usatPercentage"               "usatPercentageAhead"         
#> [11] "usatPercentageTied"           "usatPercentageBehind"        
#> [13] "usatPrecentageClose"          "usatRelative"                
#> [15] "zoneStartPct5v5"              "shootingPct5v5"              
#> [17] "skaterSavePct5v5"             "skaterShootingPlusSavePct5v5"
#> 
#> $playerReportData$percentages$game$sortKeys
#> [1] "satPercentage"
#> 
#> 
#> $playerReportData$percentages$season
#> $playerReportData$percentages$season$displayItems
#>  [1] "playerId"                     "skaterFullName"              
#>  [3] "seasonId"                     "teamAbbrevs"                 
#>  [5] "shootsCatches"                "positionCode"                
#>  [7] "gamesPlayed"                  "timeOnIcePerGame5v5"         
#>  [9] "satPercentage"                "satPercentageAhead"          
#> [11] "satPercentageTied"            "satPercentageBehind"         
#> [13] "satPercentageClose"           "satRelative"                 
#> [15] "usatPercentage"               "usatPercentageAhead"         
#> [17] "usatPercentageTied"           "usatPercentageBehind"        
#> [19] "usatPrecentageClose"          "usatRelative"                
#> [21] "zoneStartPct5v5"              "shootingPct5v5"              
#> [23] "skaterSavePct5v5"             "skaterShootingPlusSavePct5v5"
#> 
#> $playerReportData$percentages$season$resultFilters
#>  [1] "gamesPlayed"                  "timeOnIcePerGame5v5"         
#>  [3] "satPercentage"                "satPercentageAhead"          
#>  [5] "satPercentageTied"            "satPercentageBehind"         
#>  [7] "satPercentageClose"           "satRelative"                 
#>  [9] "usatPercentage"               "usatPercentageAhead"         
#> [11] "usatPercentageTied"           "usatPercentageBehind"        
#> [13] "usatPrecentageClose"          "usatRelative"                
#> [15] "zoneStartPct5v5"              "shootingPct5v5"              
#> [17] "skaterSavePct5v5"             "skaterShootingPlusSavePct5v5"
#> 
#> $playerReportData$percentages$season$sortKeys
#> [1] "satPercentage"
#> 
#> 
#> 
#> $playerReportData$scoringRates
#> $playerReportData$scoringRates$game
#> $playerReportData$scoringRates$game$displayItems
#>  [1] "playerId"                 "skaterFullName"          
#>  [3] "gameId"                   "opponentTeamAbbrev"      
#>  [5] "gameDate"                 "teamAbbrev"              
#>  [7] "positionCode"             "gamesPlayed"             
#>  [9] "timeOnIcePerGame5v5"      "goals5v5"                
#> [11] "assists5v5"               "primaryAssists5v5"       
#> [13] "secondaryAssists5v5"      "points5v5"               
#> [15] "goalsPer605v5"            "assistsPer605v5"         
#> [17] "primaryAssistsPer605v5"   "secondaryAssistsPer605v5"
#> [19] "pointsPer605v5"           "shootingPct5v5"          
#> [21] "onIceShootingPct5v5"      "offensiveZoneStartPct5v5"
#> [23] "satRelative5v5"           "satPct"                  
#> [25] "netMinorPenaltiesPer60"  
#> 
#> $playerReportData$scoringRates$game$resultFilters
#>  [1] "gamesPlayed"              "timeOnIcePerGame5v5"     
#>  [3] "goals5v5"                 "assists5v5"              
#>  [5] "primaryAssists5v5"        "secondaryAssists5v5"     
#>  [7] "points5v5"                "goalsPer605v5"           
#>  [9] "assistsPer605v5"          "primaryAssistsPer605v5"  
#> [11] "secondaryAssistsPer605v5" "pointsPer605v5"          
#> [13] "shootingPct5v5"           "onIceShootingPct5v5"     
#> [15] "offensiveZoneStartPct5v5" "satRelative5v5"          
#> [17] "satPct"                   "netMinorPenaltiesPer60"  
#> 
#> $playerReportData$scoringRates$game$sortKeys
#> [1] "pointsPer605v5" "goalsPer605v5" 
#> 
#> 
#> $playerReportData$scoringRates$season
#> $playerReportData$scoringRates$season$displayItems
#>  [1] "playerId"                 "skaterFullName"          
#>  [3] "seasonId"                 "teamAbbrevs"             
#>  [5] "positionCode"             "gamesPlayed"             
#>  [7] "timeOnIcePerGame5v5"      "goals5v5"                
#>  [9] "assists5v5"               "primaryAssists5v5"       
#> [11] "secondaryAssists5v5"      "points5v5"               
#> [13] "goalsPer605v5"            "assistsPer605v5"         
#> [15] "primaryAssistsPer605v5"   "secondaryAssistsPer605v5"
#> [17] "pointsPer605v5"           "shootingPct5v5"          
#> [19] "onIceShootingPct5v5"      "offensiveZoneStartPct5v5"
#> [21] "satRelative5v5"           "satPct"                  
#> [23] "netMinorPenaltiesPer60"  
#> 
#> $playerReportData$scoringRates$season$resultFilters
#>  [1] "gamesPlayed"              "timeOnIcePerGame5v5"     
#>  [3] "goals5v5"                 "assists5v5"              
#>  [5] "primaryAssists5v5"        "secondaryAssists5v5"     
#>  [7] "points5v5"                "goalsPer605v5"           
#>  [9] "assistsPer605v5"          "primaryAssistsPer605v5"  
#> [11] "secondaryAssistsPer605v5" "pointsPer605v5"          
#> [13] "shootingPct5v5"           "onIceShootingPct5v5"     
#> [15] "offensiveZoneStartPct5v5" "satRelative5v5"          
#> [17] "satPct"                   "netMinorPenaltiesPer60"  
#> 
#> $playerReportData$scoringRates$season$sortKeys
#> [1] "pointsPer605v5" "goalsPer605v5" 
#> 
#> 
#> 
#> $playerReportData$penaltyShots
#> $playerReportData$penaltyShots$game
#> $playerReportData$penaltyShots$game$displayItems
#>  [1] "playerId"               "skaterFullName"         "gameId"                
#>  [4] "opponentTeamAbbrev"     "gameDate"               "teamAbbrev"            
#>  [7] "shootsCatches"          "positionCode"           "penaltyShotAttempts"   
#> [10] "penaltyShotsGoals"      "penaltyShotsFailed"     "penaltyShotShootingPct"
#> 
#> $playerReportData$penaltyShots$game$resultFilters
#> [1] "penaltyShotAttempts"    "penaltyShotsGoals"      "penaltyShotsFailed"    
#> [4] "penaltyShotShootingPct"
#> 
#> $playerReportData$penaltyShots$game$sortKeys
#> [1] "penaltyShotsGoals"
#> 
#> 
#> $playerReportData$penaltyShots$season
#> $playerReportData$penaltyShots$season$displayItems
#>  [1] "playerId"               "skaterFullName"         "seasonId"              
#>  [4] "teamAbbrevs"            "shootsCatches"          "positionCode"          
#>  [7] "penaltyShotAttempts"    "penaltyShotsGoals"      "penaltyShotsFailed"    
#> [10] "penaltyShotShootingPct"
#> 
#> $playerReportData$penaltyShots$season$resultFilters
#> [1] "penaltyShotAttempts"    "penaltyShotsGoals"      "penaltyShotsFailed"    
#> [4] "penaltyShotShootingPct"
#> 
#> $playerReportData$penaltyShots$season$sortKeys
#> [1] "penaltyShotsGoals"
#> 
#> 
#> 
#> $playerReportData$puckPossessions
#> $playerReportData$puckPossessions$game
#> $playerReportData$puckPossessions$game$displayItems
#>  [1] "playerId"                "skaterFullName"         
#>  [3] "gameId"                  "opponentTeamAbbrev"     
#>  [5] "gameDate"                "teamAbbrev"             
#>  [7] "shootsCatches"           "positionCode"           
#>  [9] "gamesPlayed"             "timeOnIcePerGame5v5"    
#> [11] "satPct"                  "usatPct"                
#> [13] "goalsPct"                "individualSatForPer60"  
#> [15] "individualShotsForPer60" "onIceShootingPct"       
#> [17] "offensiveZoneStartRatio" "offensiveZoneStartPct"  
#> [19] "neutralZoneStartPct"     "defensiveZoneStartPct"  
#> [21] "faceoffPct5v5"          
#> 
#> $playerReportData$puckPossessions$game$resultFilters
#>  [1] "gamesPlayed"             "timeOnIcePerGame5v5"    
#>  [3] "satPct"                  "usatPct"                
#>  [5] "goalsPct"                "individualSatForPer60"  
#>  [7] "individualShotsForPer60" "onIceShootingPct"       
#>  [9] "offensiveZoneStartRatio" "offensiveZoneStartPct"  
#> [11] "neutralZoneStartPct"     "defensiveZoneStartPct"  
#> [13] "faceoffPct5v5"          
#> 
#> $playerReportData$puckPossessions$game$sortKeys
#> [1] "satPct"
#> 
#> 
#> $playerReportData$puckPossessions$season
#> $playerReportData$puckPossessions$season$displayItems
#>  [1] "playerId"                "skaterFullName"         
#>  [3] "seasonId"                "teamAbbrevs"            
#>  [5] "shootsCatches"           "positionCode"           
#>  [7] "gamesPlayed"             "timeOnIcePerGame5v5"    
#>  [9] "satPct"                  "usatPct"                
#> [11] "goalsPct"                "individualSatForPer60"  
#> [13] "individualShotsForPer60" "onIceShootingPct"       
#> [15] "offensiveZoneStartRatio" "offensiveZoneStartPct"  
#> [17] "neutralZoneStartPct"     "defensiveZoneStartPct"  
#> [19] "faceoffPct5v5"          
#> 
#> $playerReportData$puckPossessions$season$resultFilters
#>  [1] "gamesPlayed"             "timeOnIcePerGame5v5"    
#>  [3] "satPct"                  "usatPct"                
#>  [5] "goalsPct"                "individualSatForPer60"  
#>  [7] "individualShotsForPer60" "onIceShootingPct"       
#>  [9] "offensiveZoneStartRatio" "offensiveZoneStartPct"  
#> [11] "neutralZoneStartPct"     "defensiveZoneStartPct"  
#> [13] "faceoffPct5v5"          
#> 
#> $playerReportData$puckPossessions$season$sortKeys
#> [1] "satPct"
#> 
#> 
#> 
#> $playerReportData$timeonice
#> $playerReportData$timeonice$game
#> $playerReportData$timeonice$game$displayItems
#>  [1] "playerId"             "skaterFullName"       "gameId"              
#>  [4] "opponentTeamAbbrev"   "gameDate"             "teamAbbrev"          
#>  [7] "shootsCatches"        "positionCode"         "gamesPlayed"         
#> [10] "timeOnIce"            "evTimeOnIce"          "ppTimeOnIce"         
#> [13] "shTimeOnIce"          "timeOnIcePerGame"     "evTimeOnIcePerGame"  
#> [16] "ppTimeOnIcePerGame"   "shTimeOnIcePerGame"   "otTimeOnIce"         
#> [19] "otTimeOnIcePerOtGame" "shifts"               "timeOnIcePerShift"   
#> [22] "shiftsPerGame"       
#> 
#> $playerReportData$timeonice$game$resultFilters
#>  [1] "gamesPlayed"          "timeOnIce"            "evTimeOnIce"         
#>  [4] "ppTimeOnIce"          "shTimeOnIce"          "timeOnIcePerGame"    
#>  [7] "evTimeOnIcePerGame"   "ppTimeOnIcePerGame"   "shTimeOnIcePerGame"  
#> [10] "otTimeOnIce"          "otTimeOnIcePerOtGame" "shifts"              
#> [13] "timeOnIcePerShift"    "shiftsPerGame"       
#> 
#> $playerReportData$timeonice$game$sortKeys
#> [1] "timeOnIce"
#> 
#> 
#> $playerReportData$timeonice$season
#> $playerReportData$timeonice$season$displayItems
#>  [1] "playerId"             "skaterFullName"       "seasonId"            
#>  [4] "teamAbbrevs"          "shootsCatches"        "positionCode"        
#>  [7] "gamesPlayed"          "timeOnIce"            "evTimeOnIce"         
#> [10] "ppTimeOnIce"          "shTimeOnIce"          "timeOnIcePerGame"    
#> [13] "evTimeOnIcePerGame"   "ppTimeOnIcePerGame"   "shTimeOnIcePerGame"  
#> [16] "otTimeOnIce"          "otTimeOnIcePerOtGame" "shifts"              
#> [19] "timeOnIcePerShift"    "shiftsPerGame"       
#> 
#> $playerReportData$timeonice$season$resultFilters
#>  [1] "gamesPlayed"          "timeOnIce"            "evTimeOnIce"         
#>  [4] "ppTimeOnIce"          "shTimeOnIce"          "timeOnIcePerGame"    
#>  [7] "evTimeOnIcePerGame"   "ppTimeOnIcePerGame"   "shTimeOnIcePerGame"  
#> [10] "otTimeOnIce"          "otTimeOnIcePerOtGame" "shifts"              
#> [13] "timeOnIcePerShift"    "shiftsPerGame"       
#> 
#> $playerReportData$timeonice$season$sortKeys
#> [1] "timeOnIce"
#> 
#> 
#> 
#> $playerReportData$summaryshooting
#> $playerReportData$summaryshooting$game
#> $playerReportData$summaryshooting$game$displayItems
#>  [1] "playerId"            "skaterFullName"      "gameId"             
#>  [4] "opponentTeamAbbrev"  "gameDate"            "shootsCatches"      
#>  [7] "positionCode"        "gamesPlayed"         "timeOnIcePerGame5v5"
#> [10] "satFor"              "satAgainst"          "satTotal"           
#> [13] "satAhead"            "satTied"             "satBehind"          
#> [16] "satClose"            "satRelative"         "usatFor"            
#> [19] "usatAgainst"         "usatTotal"           "usatAhead"          
#> [22] "usatTied"            "usatBehind"          "usatClose"          
#> [25] "usatRelative"       
#> 
#> $playerReportData$summaryshooting$game$resultFilters
#>  [1] "gamesPlayed"         "timeOnIcePerGame5v5" "satFor"             
#>  [4] "satAgainst"          "satTotal"            "satAhead"           
#>  [7] "satTied"             "satBehind"           "satClose"           
#> [10] "satRelative"         "usatFor"             "usatAgainst"        
#> [13] "usatTotal"           "usatAhead"           "usatTied"           
#> [16] "usatBehind"          "usatClose"           "usatRelative"       
#> 
#> $playerReportData$summaryshooting$game$sortKeys
#> [1] "satTotal"  "usatTotal"
#> 
#> 
#> $playerReportData$summaryshooting$season
#> $playerReportData$summaryshooting$season$displayItems
#>  [1] "playerId"            "skaterFullName"      "seasonId"           
#>  [4] "teamAbbrevs"         "shootsCatches"       "positionCode"       
#>  [7] "gamesPlayed"         "timeOnIcePerGame5v5" "satFor"             
#> [10] "satAgainst"          "satTotal"            "satAhead"           
#> [13] "satTied"             "satBehind"           "satClose"           
#> [16] "satRelative"         "usatFor"             "usatAgainst"        
#> [19] "usatTotal"           "usatAhead"           "usatTied"           
#> [22] "usatBehind"          "usatClose"           "usatRelative"       
#> 
#> $playerReportData$summaryshooting$season$resultFilters
#>  [1] "gamesPlayed"         "timeOnIcePerGame5v5" "satFor"             
#>  [4] "satAgainst"          "satTotal"            "satAhead"           
#>  [7] "satTied"             "satBehind"           "satClose"           
#> [10] "satRelative"         "usatFor"             "usatAgainst"        
#> [13] "usatTotal"           "usatAhead"           "usatTied"           
#> [16] "usatBehind"          "usatClose"           "usatRelative"       
#> 
#> $playerReportData$summaryshooting$season$sortKeys
#> [1] "satTotal"  "usatTotal"
#> 
#> 
#> 
#> $playerReportData$powerplay
#> $playerReportData$powerplay$game
#> $playerReportData$powerplay$game$displayItems
#>  [1] "playerId"                "skaterFullName"         
#>  [3] "gameId"                  "opponentTeamAbbrev"     
#>  [5] "gameDate"                "teamAbbrev"             
#>  [7] "positionCode"            "gamesPlayed"            
#>  [9] "ppGoals"                 "ppAssists"              
#> [11] "ppPrimaryAssists"        "ppSecondaryAssists"     
#> [13] "ppPoints"                "ppIndividualSatFor"     
#> [15] "ppShots"                 "ppShootingPct"          
#> [17] "ppGoalsPer60"            "ppPrimaryAssistsPer60"  
#> [19] "ppSecondaryAssistsPer60" "ppPointsPer60"          
#> [21] "ppIndividualSatForPer60" "ppShotsPer60"           
#> [23] "ppGoalsForPer60"         "ppTimeOnIce"            
#> [25] "ppTimeOnIcePerGame"      "ppTimeOnIcePctPerGame"  
#> 
#> $playerReportData$powerplay$game$resultFilters
#>  [1] "gamesPlayed"             "ppGoals"                
#>  [3] "ppAssists"               "ppPrimaryAssists"       
#>  [5] "ppSecondaryAssists"      "ppPoints"               
#>  [7] "ppIndividualSatFor"      "ppShots"                
#>  [9] "ppShootingPct"           "ppGoalsPer60"           
#> [11] "ppPrimaryAssistsPer60"   "ppSecondaryAssistsPer60"
#> [13] "ppPointsPer60"           "ppIndividualSatForPer60"
#> [15] "ppShotsPer60"            "ppGoalsForPer60"        
#> [17] "ppTimeOnIce"             "ppTimeOnIcePerGame"     
#> [19] "ppTimeOnIcePctPerGame"  
#> 
#> $playerReportData$powerplay$game$sortKeys
#> [1] "ppTimeOnIce"
#> 
#> 
#> $playerReportData$powerplay$season
#> $playerReportData$powerplay$season$displayItems
#>  [1] "playerId"                "skaterFullName"         
#>  [3] "seasonId"                "teamAbbrevs"            
#>  [5] "positionCode"            "gamesPlayed"            
#>  [7] "ppGoals"                 "ppAssists"              
#>  [9] "ppPrimaryAssists"        "ppSecondaryAssists"     
#> [11] "ppPoints"                "ppIndividualSatFor"     
#> [13] "ppShots"                 "ppShootingPct"          
#> [15] "ppGoalsPer60"            "ppPrimaryAssistsPer60"  
#> [17] "ppSecondaryAssistsPer60" "ppPointsPer60"          
#> [19] "ppIndividualSatForPer60" "ppShotsPer60"           
#> [21] "ppGoalsForPer60"         "ppTimeOnIce"            
#> [23] "ppTimeOnIcePerGame"      "ppTimeOnIcePctPerGame"  
#> 
#> $playerReportData$powerplay$season$resultFilters
#>  [1] "gamesPlayed"             "ppGoals"                
#>  [3] "ppAssists"               "ppPrimaryAssists"       
#>  [5] "ppSecondaryAssists"      "ppPoints"               
#>  [7] "ppIndividualSatFor"      "ppShots"                
#>  [9] "ppShootingPct"           "ppGoalsPer60"           
#> [11] "ppPrimaryAssistsPer60"   "ppSecondaryAssistsPer60"
#> [13] "ppPointsPer60"           "ppIndividualSatForPer60"
#> [15] "ppShotsPer60"            "ppGoalsForPer60"        
#> [17] "ppTimeOnIce"             "ppTimeOnIcePerGame"     
#> [19] "ppTimeOnIcePctPerGame"  
#> 
#> $playerReportData$powerplay$season$sortKeys
#> [1] "ppTimeOnIce"
#> 
#> 
#> 
#> $playerReportData$goalsForAgainst
#> $playerReportData$goalsForAgainst$game
#> $playerReportData$goalsForAgainst$game$displayItems
#>  [1] "playerId"                     "skaterFullName"              
#>  [3] "gameId"                       "opponentTeamAbbrev"          
#>  [5] "gameDate"                     "teamAbbrev"                  
#>  [7] "positionCode"                 "gamesPlayed"                 
#>  [9] "goals"                        "assists"                     
#> [11] "points"                       "powerPlayTimeOnIcePerGame"   
#> [13] "powerPlayGoalFor"             "shortHandedGoalsAgainst"     
#> [15] "shortHandedTimeOnIcePerGame"  "shortHandedGoalsFor"         
#> [17] "powerPlayGoalsAgainst"        "evenStrengthTimeOnIcePerGame"
#> [19] "evenStrengthGoalsFor"         "evenStrengthGoalsAgainst"    
#> [21] "evenStrengthGoalDifference"   "evenStrengthGoalsForPct"     
#> 
#> $playerReportData$goalsForAgainst$game$resultFilters
#>  [1] "gamesPlayed"                  "goals"                       
#>  [3] "assists"                      "points"                      
#>  [5] "powerPlayTimeOnIcePerGame"    "powerPlayGoalFor"            
#>  [7] "shortHandedGoalsAgainst"      "shortHandedTimeOnIcePerGame" 
#>  [9] "shortHandedGoalsFor"          "powerPlayGoalsAgainst"       
#> [11] "evenStrengthTimeOnIcePerGame" "evenStrengthGoalsFor"        
#> [13] "evenStrengthGoalsAgainst"     "evenStrengthGoalDifference"  
#> [15] "evenStrengthGoalsForPct"     
#> 
#> $playerReportData$goalsForAgainst$game$sortKeys
#> [1] "evenStrengthGoalDifference"
#> 
#> 
#> $playerReportData$goalsForAgainst$season
#> $playerReportData$goalsForAgainst$season$displayItems
#>  [1] "playerId"                     "skaterFullName"              
#>  [3] "seasonId"                     "teamAbbrevs"                 
#>  [5] "positionCode"                 "gamesPlayed"                 
#>  [7] "goals"                        "assists"                     
#>  [9] "points"                       "powerPlayTimeOnIcePerGame"   
#> [11] "powerPlayGoalFor"             "shortHandedGoalsAgainst"     
#> [13] "shortHandedTimeOnIcePerGame"  "shortHandedGoalsFor"         
#> [15] "powerPlayGoalsAgainst"        "evenStrengthTimeOnIcePerGame"
#> [17] "evenStrengthGoalsFor"         "evenStrengthGoalsAgainst"    
#> [19] "evenStrengthGoalDifference"   "evenStrengthGoalsForPct"     
#> 
#> $playerReportData$goalsForAgainst$season$resultFilters
#>  [1] "gamesPlayed"                  "goals"                       
#>  [3] "assists"                      "points"                      
#>  [5] "powerPlayTimeOnIcePerGame"    "powerPlayGoalFor"            
#>  [7] "shortHandedGoalsAgainst"      "shortHandedTimeOnIcePerGame" 
#>  [9] "shortHandedGoalsFor"          "powerPlayGoalsAgainst"       
#> [11] "evenStrengthTimeOnIcePerGame" "evenStrengthGoalsFor"        
#> [13] "evenStrengthGoalsAgainst"     "evenStrengthGoalDifference"  
#> [15] "evenStrengthGoalsForPct"     
#> 
#> $playerReportData$goalsForAgainst$season$sortKeys
#> [1] "evenStrengthGoalDifference"
#> 
#> 
#> 
#> $playerReportData$scoringpergame
#> $playerReportData$scoringpergame$game
#> $playerReportData$scoringpergame$game$displayItems
#>  [1] "playerId"                "skaterFullName"         
#>  [3] "gameId"                  "opponentTeamAbbrev"     
#>  [5] "gameDate"                "teamAbbrev"             
#>  [7] "shootsCatches"           "positionCode"           
#>  [9] "gamesPlayed"             "goals"                  
#> [11] "assists"                 "totalPrimaryAssists"    
#> [13] "totalSecondaryAssists"   "points"                 
#> [15] "shots"                   "penaltyMinutes"         
#> [17] "hits"                    "blockedShots"           
#> [19] "timeOnIce"               "goalsPerGame"           
#> [21] "assistsPerGame"          "primaryAssistsPerGame"  
#> [23] "secondaryAssistsPerGame" "pointsPerGame"          
#> [25] "shotsPerGame"            "penaltyMinutesPerGame"  
#> [27] "hitsPerGame"             "blocksPerGame"          
#> [29] "timeOnIcePerGame"       
#> 
#> $playerReportData$scoringpergame$game$resultFilters
#>  [1] "gamesPlayed"             "goals"                  
#>  [3] "assists"                 "totalPrimaryAssists"    
#>  [5] "totalSecondaryAssists"   "points"                 
#>  [7] "shots"                   "penaltyMinutes"         
#>  [9] "hits"                    "blockedShots"           
#> [11] "timeOnIce"               "goalsPerGame"           
#> [13] "assistsPerGame"          "primaryAssistsPerGame"  
#> [15] "secondaryAssistsPerGame" "pointsPerGame"          
#> [17] "shotsPerGame"            "penaltyMinutesPerGame"  
#> [19] "hitsPerGame"             "blocksPerGame"          
#> [21] "timeOnIcePerGame"       
#> 
#> $playerReportData$scoringpergame$game$sortKeys
#> [1] "pointsPerGame" "goalsPerGame" 
#> 
#> 
#> $playerReportData$scoringpergame$season
#> $playerReportData$scoringpergame$season$displayItems
#>  [1] "playerId"                "skaterFullName"         
#>  [3] "seasonId"                "teamAbbrevs"            
#>  [5] "shootsCatches"           "positionCode"           
#>  [7] "gamesPlayed"             "goals"                  
#>  [9] "assists"                 "totalPrimaryAssists"    
#> [11] "totalSecondaryAssists"   "points"                 
#> [13] "shots"                   "penaltyMinutes"         
#> [15] "hits"                    "blockedShots"           
#> [17] "timeOnIce"               "goalsPerGame"           
#> [19] "assistsPerGame"          "primaryAssistsPerGame"  
#> [21] "secondaryAssistsPerGame" "pointsPerGame"          
#> [23] "shotsPerGame"            "penaltyMinutesPerGame"  
#> [25] "hitsPerGame"             "blocksPerGame"          
#> [27] "timeOnIcePerGame"       
#> 
#> $playerReportData$scoringpergame$season$resultFilters
#>  [1] "gamesPlayed"             "goals"                  
#>  [3] "assists"                 "totalPrimaryAssists"    
#>  [5] "totalSecondaryAssists"   "points"                 
#>  [7] "shots"                   "penaltyMinutes"         
#>  [9] "hits"                    "blockedShots"           
#> [11] "timeOnIce"               "goalsPerGame"           
#> [13] "assistsPerGame"          "primaryAssistsPerGame"  
#> [15] "secondaryAssistsPerGame" "pointsPerGame"          
#> [17] "shotsPerGame"            "penaltyMinutesPerGame"  
#> [19] "hitsPerGame"             "blocksPerGame"          
#> [21] "timeOnIcePerGame"       
#> 
#> $playerReportData$scoringpergame$season$sortKeys
#> [1] "pointsPerGame" "goalsPerGame" 
#> 
#> 
#> 
#> $playerReportData$penaltykill
#> $playerReportData$penaltykill$game
#> $playerReportData$penaltykill$game$displayItems
#>  [1] "playerId"                "skaterFullName"         
#>  [3] "gameId"                  "opponentTeamAbbrev"     
#>  [5] "gameDate"                "teamAbbrev"             
#>  [7] "positionCode"            "gamesPlayed"            
#>  [9] "shGoals"                 "shAssists"              
#> [11] "shPrimaryAssists"        "shSecondaryAssists"     
#> [13] "shPoints"                "shIndividualSatFor"     
#> [15] "shShots"                 "shShootingPct"          
#> [17] "shGoalsPer60"            "shPrimaryAssistsPer60"  
#> [19] "shSecondaryAssistsPer60" "shPointsPer60"          
#> [21] "shIndividualSatForPer60" "shShotsPer60"           
#> [23] "ppGoalsAgainstPer60"     "shTimeOnIce"            
#> [25] "shTimeOnIcePerGame"      "shTimeOnIcePctPerGame"  
#> 
#> $playerReportData$penaltykill$game$resultFilters
#>  [1] "gamesPlayed"             "shGoals"                
#>  [3] "shAssists"               "shPrimaryAssists"       
#>  [5] "shSecondaryAssists"      "shPoints"               
#>  [7] "shIndividualSatFor"      "shShots"                
#>  [9] "shShootingPct"           "shGoalsPer60"           
#> [11] "shPrimaryAssistsPer60"   "shSecondaryAssistsPer60"
#> [13] "shPointsPer60"           "shIndividualSatForPer60"
#> [15] "shShotsPer60"            "ppGoalsAgainstPer60"    
#> [17] "shTimeOnIce"             "shTimeOnIcePerGame"     
#> [19] "shTimeOnIcePctPerGame"  
#> 
#> $playerReportData$penaltykill$game$sortKeys
#> [1] "shTimeOnIce"
#> 
#> 
#> $playerReportData$penaltykill$season
#> $playerReportData$penaltykill$season$displayItems
#>  [1] "playerId"                "skaterFullName"         
#>  [3] "seasonId"                "teamAbbrevs"            
#>  [5] "positionCode"            "gamesPlayed"            
#>  [7] "shGoals"                 "shAssists"              
#>  [9] "shPrimaryAssists"        "shSecondaryAssists"     
#> [11] "shPoints"                "shIndividualSatFor"     
#> [13] "shShots"                 "shShootingPct"          
#> [15] "shGoalsPer60"            "shPrimaryAssistsPer60"  
#> [17] "shSecondaryAssistsPer60" "shPointsPer60"          
#> [19] "shIndividualSatForPer60" "shShotsPer60"           
#> [21] "ppGoalsAgainstPer60"     "shTimeOnIce"            
#> [23] "shTimeOnIcePerGame"      "shTimeOnIcePctPerGame"  
#> 
#> $playerReportData$penaltykill$season$resultFilters
#>  [1] "gamesPlayed"             "shGoals"                
#>  [3] "shAssists"               "shPrimaryAssists"       
#>  [5] "shSecondaryAssists"      "shPoints"               
#>  [7] "shIndividualSatFor"      "shShots"                
#>  [9] "shShootingPct"           "shGoalsPer60"           
#> [11] "shPrimaryAssistsPer60"   "shSecondaryAssistsPer60"
#> [13] "shPointsPer60"           "shIndividualSatForPer60"
#> [15] "shShotsPer60"            "ppGoalsAgainstPer60"    
#> [17] "shTimeOnIce"             "shTimeOnIcePerGame"     
#> [19] "shTimeOnIcePctPerGame"  
#> 
#> $playerReportData$penaltykill$season$sortKeys
#> [1] "shTimeOnIce"
#> 
#> 
#> 
#> $playerReportData$faceoffwins
#> $playerReportData$faceoffwins$game
#> $playerReportData$faceoffwins$game$displayItems
#>  [1] "playerId"                   "skaterFullName"            
#>  [3] "gameId"                     "opponentTeamAbbrev"        
#>  [5] "gameDate"                   "teamAbbrev"                
#>  [7] "positionCode"               "gamesPlayed"               
#>  [9] "totalFaceoffs"              "totalFaceoffWins"          
#> [11] "totalFaceoffLosses"         "faceoffWinPct"             
#> [13] "evFaceoffs"                 "evFaceoffsWon"             
#> [15] "evFaceoffsLost"             "ppFaceoffs"                
#> [17] "ppFaceoffsWon"              "ppFaceoffsLost"            
#> [19] "shFaceoffs"                 "shFaceoffsWon"             
#> [21] "shFaceoffsLost"             "offensiveZoneFaceoffs"     
#> [23] "offensiveZoneFaceoffWins"   "offensiveZoneFaceoffLosses"
#> [25] "neutralZoneFaceoffs"        "neutralZoneFaceoffWins"    
#> [27] "neutralZoneFaceoffLosses"   "defensiveZoneFaceoffs"     
#> [29] "defensiveZoneFaceoffWins"   "defensiveZoneFaceoffLosses"
#> 
#> $playerReportData$faceoffwins$game$resultFilters
#>  [1] "gamesPlayed"                "totalFaceoffs"             
#>  [3] "totalFaceoffWins"           "totalFaceoffLosses"        
#>  [5] "faceoffWinPct"              "evFaceoffs"                
#>  [7] "evFaceoffsWon"              "evFaceoffsLost"            
#>  [9] "ppFaceoffs"                 "ppFaceoffsWon"             
#> [11] "ppFaceoffsLost"             "shFaceoffs"                
#> [13] "shFaceoffsWon"              "shFaceoffsLost"            
#> [15] "offensiveZoneFaceoffs"      "offensiveZoneFaceoffWins"  
#> [17] "offensiveZoneFaceoffLosses" "neutralZoneFaceoffs"       
#> [19] "neutralZoneFaceoffWins"     "neutralZoneFaceoffLosses"  
#> [21] "defensiveZoneFaceoffs"      "defensiveZoneFaceoffWins"  
#> [23] "defensiveZoneFaceoffLosses"
#> 
#> $playerReportData$faceoffwins$game$sortKeys
#> [1] "totalFaceoffWins" "faceoffWinPct"   
#> 
#> 
#> $playerReportData$faceoffwins$season
#> $playerReportData$faceoffwins$season$displayItems
#>  [1] "playerId"                   "skaterFullName"            
#>  [3] "seasonId"                   "teamAbbrevs"               
#>  [5] "positionCode"               "gamesPlayed"               
#>  [7] "totalFaceoffs"              "totalFaceoffWins"          
#>  [9] "totalFaceoffLosses"         "faceoffWinPct"             
#> [11] "evFaceoffs"                 "evFaceoffsWon"             
#> [13] "evFaceoffsLost"             "ppFaceoffs"                
#> [15] "ppFaceoffsWon"              "ppFaceoffsLost"            
#> [17] "shFaceoffs"                 "shFaceoffsWon"             
#> [19] "shFaceoffsLost"             "offensiveZoneFaceoffs"     
#> [21] "offensiveZoneFaceoffWins"   "offensiveZoneFaceoffLosses"
#> [23] "neutralZoneFaceoffs"        "neutralZoneFaceoffWins"    
#> [25] "neutralZoneFaceoffLosses"   "defensiveZoneFaceoffs"     
#> [27] "defensiveZoneFaceoffWins"   "defensiveZoneFaceoffLosses"
#> 
#> $playerReportData$faceoffwins$season$resultFilters
#>  [1] "gamesPlayed"                "totalFaceoffs"             
#>  [3] "totalFaceoffWins"           "totalFaceoffLosses"        
#>  [5] "faceoffWinPct"              "evFaceoffs"                
#>  [7] "evFaceoffsWon"              "evFaceoffsLost"            
#>  [9] "ppFaceoffs"                 "ppFaceoffsWon"             
#> [11] "ppFaceoffsLost"             "shFaceoffs"                
#> [13] "shFaceoffsWon"              "shFaceoffsLost"            
#> [15] "offensiveZoneFaceoffs"      "offensiveZoneFaceoffWins"  
#> [17] "offensiveZoneFaceoffLosses" "neutralZoneFaceoffs"       
#> [19] "neutralZoneFaceoffWins"     "neutralZoneFaceoffLosses"  
#> [21] "defensiveZoneFaceoffs"      "defensiveZoneFaceoffWins"  
#> [23] "defensiveZoneFaceoffLosses"
#> 
#> $playerReportData$faceoffwins$season$sortKeys
#> [1] "totalFaceoffWins" "faceoffWinPct"   
#> 
#> 
#> 
#> 
#> $goalieReportData
#> $goalieReportData$summary
#> $goalieReportData$summary$game
#> $goalieReportData$summary$game$displayItems
#>  [1] "playerId"            "goalieFullName"      "gameId"             
#>  [4] "opponentTeamAbbrev"  "gameDate"            "teamAbbrev"         
#>  [7] "shootsCatches"       "gamesPlayed"         "gamesStarted"       
#> [10] "wins"                "losses"              "ties"               
#> [13] "otLosses"            "shotsAgainst"        "saves"              
#> [16] "goalsAgainst"        "savePct"             "goalsAgainstAverage"
#> [19] "timeOnIce"           "shutouts"            "goals"              
#> [22] "assists"             "points"              "penaltyMinutes"     
#> 
#> $goalieReportData$summary$game$resultFilters
#>  [1] "gamesPlayed"         "gamesStarted"        "wins"               
#>  [4] "losses"              "ties"                "otLosses"           
#>  [7] "shotsAgainst"        "saves"               "goalsAgainst"       
#> [10] "savePct"             "goalsAgainstAverage" "timeOnIce"          
#> [13] "shutouts"            "goals"               "assists"            
#> [16] "points"              "penaltyMinutes"     
#> 
#> $goalieReportData$summary$game$sortKeys
#> [1] "wins"    "savePct"
#> 
#> 
#> $goalieReportData$summary$season
#> $goalieReportData$summary$season$displayItems
#>  [1] "playerId"            "goalieFullName"      "seasonId"           
#>  [4] "teamAbbrevs"         "shootsCatches"       "gamesPlayed"        
#>  [7] "gamesStarted"        "wins"                "losses"             
#> [10] "ties"                "otLosses"            "shotsAgainst"       
#> [13] "saves"               "goalsAgainst"        "savePct"            
#> [16] "goalsAgainstAverage" "timeOnIce"           "shutouts"           
#> [19] "goals"               "assists"             "points"             
#> [22] "penaltyMinutes"     
#> 
#> $goalieReportData$summary$season$resultFilters
#>  [1] "gamesPlayed"         "gamesStarted"        "wins"               
#>  [4] "losses"              "ties"                "otLosses"           
#>  [7] "shotsAgainst"        "saves"               "goalsAgainst"       
#> [10] "savePct"             "goalsAgainstAverage" "timeOnIce"          
#> [13] "shutouts"            "goals"               "assists"            
#> [16] "points"              "penaltyMinutes"     
#> 
#> $goalieReportData$summary$season$sortKeys
#> [1] "wins"    "savePct"
#> 
#> 
#> 
#> $goalieReportData$shootout
#> $goalieReportData$shootout$game
#> $goalieReportData$shootout$game$displayItems
#>  [1] "playerId"                   "goalieFullName"            
#>  [3] "gameId"                     "opponentTeamAbbrev"        
#>  [5] "gameDate"                   "teamAbbrev"                
#>  [7] "shootsCatches"              "gamesPlayed"               
#>  [9] "shootoutWins"               "shootoutLosses"            
#> [11] "shootoutShotsAgainst"       "shootoutGoalsAgainst"      
#> [13] "shootoutSaves"              "shootoutSavePct"           
#> [15] "careerShootoutGamesPlayed"  "careerShootoutWins"        
#> [17] "careerShootoutLosses"       "careerShootoutShotsAgainst"
#> [19] "careerShootoutGoalsAllowed" "careerShootoutSaves"       
#> [21] "careerShootoutSavePct"     
#> 
#> $goalieReportData$shootout$game$resultFilters
#>  [1] "gamesPlayed"                "shootoutWins"              
#>  [3] "shootoutLosses"             "shootoutShotsAgainst"      
#>  [5] "shootoutGoalsAgainst"       "shootoutSaves"             
#>  [7] "shootoutSavePct"            "careerShootoutGamesPlayed" 
#>  [9] "careerShootoutWins"         "careerShootoutLosses"      
#> [11] "careerShootoutShotsAgainst" "careerShootoutGoalsAllowed"
#> [13] "careerShootoutSaves"        "careerShootoutSavePct"     
#> 
#> $goalieReportData$shootout$game$sortKeys
#> [1] "shootoutWins"    "shootoutSavePct"
#> 
#> 
#> $goalieReportData$shootout$season
#> $goalieReportData$shootout$season$displayItems
#>  [1] "playerId"                   "goalieFullName"            
#>  [3] "seasonId"                   "teamAbbrevs"               
#>  [5] "shootsCatches"              "gamesPlayed"               
#>  [7] "shootoutWins"               "shootoutLosses"            
#>  [9] "shootoutShotsAgainst"       "shootoutGoalsAgainst"      
#> [11] "shootoutSaves"              "shootoutSavePct"           
#> [13] "careerShootoutGamesPlayed"  "careerShootoutWins"        
#> [15] "careerShootoutLosses"       "careerShootoutShotsAgainst"
#> [17] "careerShootoutGoalsAllowed" "careerShootoutSaves"       
#> [19] "careerShootoutSavePct"     
#> 
#> $goalieReportData$shootout$season$resultFilters
#>  [1] "gamesPlayed"                "shootoutWins"              
#>  [3] "shootoutLosses"             "shootoutShotsAgainst"      
#>  [5] "shootoutGoalsAgainst"       "shootoutSaves"             
#>  [7] "shootoutSavePct"            "careerShootoutGamesPlayed" 
#>  [9] "careerShootoutWins"         "careerShootoutLosses"      
#> [11] "careerShootoutShotsAgainst" "careerShootoutGoalsAllowed"
#> [13] "careerShootoutSaves"        "careerShootoutSavePct"     
#> 
#> $goalieReportData$shootout$season$sortKeys
#> [1] "shootoutWins"    "shootoutSavePct"
#> 
#> 
#> 
#> $goalieReportData$advanced
#> $goalieReportData$advanced$game
#> $goalieReportData$advanced$game$displayItems
#>  [1] "playerId"            "goalieFullName"      "gameId"             
#>  [4] "opponentTeamAbbrev"  "gameDate"            "teamAbbrev"         
#>  [7] "shootsCatches"       "gamesPlayed"         "gamesStarted"       
#> [10] "completeGames"       "incompleteGames"     "completeGamePct"    
#> [13] "qualityStart"        "qualityStartsPct"    "goalsFor"           
#> [16] "goalsAgainst"        "goalsForAverage"     "goalsAgainstAverage"
#> [19] "regulationWins"      "regulationLosses"    "shotsAgainstPer60"  
#> [22] "savePct"             "timeOnIce"          
#> 
#> $goalieReportData$advanced$game$resultFilters
#>  [1] "gamesPlayed"         "gamesStarted"        "completeGames"      
#>  [4] "incompleteGames"     "completeGamePct"     "qualityStart"       
#>  [7] "qualityStartsPct"    "goalsFor"            "goalsAgainst"       
#> [10] "goalsForAverage"     "goalsAgainstAverage" "regulationWins"     
#> [13] "regulationLosses"    "shotsAgainstPer60"   "savePct"            
#> [16] "timeOnIce"          
#> 
#> $goalieReportData$advanced$game$sortKeys
#> [1] "qualityStart"        "goalsAgainstAverage"
#> 
#> 
#> $goalieReportData$advanced$season
#> $goalieReportData$advanced$season$displayItems
#>  [1] "playerId"            "goalieFullName"      "seasonId"           
#>  [4] "teamAbbrevs"         "shootsCatches"       "gamesPlayed"        
#>  [7] "gamesStarted"        "completeGames"       "incompleteGames"    
#> [10] "completeGamePct"     "qualityStart"        "qualityStartsPct"   
#> [13] "goalsFor"            "goalsAgainst"        "goalsForAverage"    
#> [16] "goalsAgainstAverage" "regulationWins"      "regulationLosses"   
#> [19] "shotsAgainstPer60"   "savePct"             "timeOnIce"          
#> 
#> $goalieReportData$advanced$season$resultFilters
#>  [1] "gamesPlayed"         "gamesStarted"        "completeGames"      
#>  [4] "incompleteGames"     "completeGamePct"     "qualityStart"       
#>  [7] "qualityStartsPct"    "goalsFor"            "goalsAgainst"       
#> [10] "goalsForAverage"     "goalsAgainstAverage" "regulationWins"     
#> [13] "regulationLosses"    "shotsAgainstPer60"   "savePct"            
#> [16] "timeOnIce"          
#> 
#> $goalieReportData$advanced$season$sortKeys
#> [1] "qualityStart"        "goalsAgainstAverage"
#> 
#> 
#> 
#> $goalieReportData$bios
#> $goalieReportData$bios$game
#> $goalieReportData$bios$game$displayItems
#>  [1] "playerId"               "goalieFullName"         "currentTeamAbbrev"     
#>  [4] "shootsCatches"          "birthDate"              "birthCity"             
#>  [7] "birthStateProvinceCode" "birthCountryCode"       "nationalityCode"       
#> [10] "height"                 "weight"                 "draftYear"             
#> [13] "draftRound"             "draftOverall"           "isInHallOfFameYn"      
#> [16] "gamesPlayed"            "wins"                   "losses"                
#> [19] "ties"                   "otLosses"               "shutouts"              
#> 
#> $goalieReportData$bios$game$resultFilters
#>  [1] "height"       "weight"       "draftYear"    "draftRound"   "draftOverall"
#>  [6] "gamesPlayed"  "wins"         "losses"       "ties"         "otLosses"    
#> [11] "shutouts"    
#> 
#> $goalieReportData$bios$game$sortKeys
#> [1] "goalieFullName"
#> 
#> 
#> $goalieReportData$bios$season
#> $goalieReportData$bios$season$displayItems
#>  [1] "playerId"               "goalieFullName"         "currentTeamAbbrev"     
#>  [4] "shootsCatches"          "birthDate"              "birthCity"             
#>  [7] "birthStateProvinceCode" "birthCountryCode"       "nationalityCode"       
#> [10] "height"                 "weight"                 "draftYear"             
#> [13] "draftRound"             "draftOverall"           "firstSeasonForGameType"
#> [16] "isInHallOfFameYn"       "gamesPlayed"            "wins"                  
#> [19] "losses"                 "ties"                   "otLosses"              
#> [22] "shutouts"              
#> 
#> $goalieReportData$bios$season$resultFilters
#>  [1] "height"       "weight"       "draftYear"    "draftRound"   "draftOverall"
#>  [6] "gamesPlayed"  "wins"         "losses"       "ties"         "otLosses"    
#> [11] "shutouts"    
#> 
#> $goalieReportData$bios$season$sortKeys
#> [1] "goalieFullName"
#> 
#> 
#> 
#> $goalieReportData$startedVsRelieved
#> $goalieReportData$startedVsRelieved$game
#> $goalieReportData$startedVsRelieved$game$displayItems
#>  [1] "playerId"                  "goalieFullName"           
#>  [3] "gameId"                    "opponentTeamAbbrev"       
#>  [5] "gameDate"                  "teamAbbrev"               
#>  [7] "shootsCatches"             "gamesPlayed"              
#>  [9] "wins"                      "losses"                   
#> [11] "ties"                      "otLosses"                 
#> [13] "savePct"                   "gamesStarted"             
#> [15] "gamesStartedWins"          "gamesStartedLosses"       
#> [17] "gamesStartedTies"          "gamesStartedOtLosses"     
#> [19] "gamesStartedShotsAgainst"  "gamesStartedSaves"        
#> [21] "gamesStartedGoalsAgainst"  "gamesStartedSavePct"      
#> [23] "gamesRelieved"             "gamesRelievedWins"        
#> [25] "gamesRelievedLosses"       "gamesRelievedTies"        
#> [27] "gamesRelievedOtLosses"     "gamesRelievedShotsAgainst"
#> [29] "gamesRelievedSaves"        "gamesRelievedGoalsAgainst"
#> [31] "gamesRelievedSavePct"     
#> 
#> $goalieReportData$startedVsRelieved$game$resultFilters
#>  [1] "gamesPlayed"               "wins"                     
#>  [3] "losses"                    "ties"                     
#>  [5] "otLosses"                  "savePct"                  
#>  [7] "gamesStarted"              "gamesStartedWins"         
#>  [9] "gamesStartedLosses"        "gamesStartedTies"         
#> [11] "gamesStartedOtLosses"      "gamesStartedShotsAgainst" 
#> [13] "gamesStartedSaves"         "gamesStartedGoalsAgainst" 
#> [15] "gamesStartedSavePct"       "gamesRelieved"            
#> [17] "gamesRelievedWins"         "gamesRelievedLosses"      
#> [19] "gamesRelievedTies"         "gamesRelievedOtLosses"    
#> [21] "gamesRelievedShotsAgainst" "gamesRelievedSaves"       
#> [23] "gamesRelievedGoalsAgainst" "gamesRelievedSavePct"     
#> 
#> $goalieReportData$startedVsRelieved$game$sortKeys
#> [1] "gamesStarted"        "gamesStartedSavePct"
#> 
#> 
#> $goalieReportData$startedVsRelieved$season
#> $goalieReportData$startedVsRelieved$season$displayItems
#>  [1] "playerId"                  "goalieFullName"           
#>  [3] "seasonId"                  "teamAbbrevs"              
#>  [5] "shootsCatches"             "gamesPlayed"              
#>  [7] "wins"                      "losses"                   
#>  [9] "ties"                      "otLosses"                 
#> [11] "savePct"                   "gamesStarted"             
#> [13] "gamesStartedWins"          "gamesStartedLosses"       
#> [15] "gamesStartedTies"          "gamesStartedOtLosses"     
#> [17] "gamesStartedShotsAgainst"  "gamesStartedSaves"        
#> [19] "gamesStartedGoalsAgainst"  "gamesStartedSavePct"      
#> [21] "gamesRelieved"             "gamesRelievedWins"        
#> [23] "gamesRelievedLosses"       "gamesRelievedTies"        
#> [25] "gamesRelievedOtLosses"     "gamesRelievedShotsAgainst"
#> [27] "gamesRelievedSaves"        "gamesRelievedGoalsAgainst"
#> [29] "gamesRelievedSavePct"     
#> 
#> $goalieReportData$startedVsRelieved$season$resultFilters
#>  [1] "gamesPlayed"               "wins"                     
#>  [3] "losses"                    "ties"                     
#>  [5] "otLosses"                  "savePct"                  
#>  [7] "gamesStarted"              "gamesStartedWins"         
#>  [9] "gamesStartedLosses"        "gamesStartedTies"         
#> [11] "gamesStartedOtLosses"      "gamesStartedShotsAgainst" 
#> [13] "gamesStartedSaves"         "gamesStartedGoalsAgainst" 
#> [15] "gamesStartedSavePct"       "gamesRelieved"            
#> [17] "gamesRelievedWins"         "gamesRelievedLosses"      
#> [19] "gamesRelievedTies"         "gamesRelievedOtLosses"    
#> [21] "gamesRelievedShotsAgainst" "gamesRelievedSaves"       
#> [23] "gamesRelievedGoalsAgainst" "gamesRelievedSavePct"     
#> 
#> $goalieReportData$startedVsRelieved$season$sortKeys
#> [1] "gamesStarted"        "gamesStartedSavePct"
#> 
#> 
#> 
#> $goalieReportData$savesByStrength
#> $goalieReportData$savesByStrength$game
#> $goalieReportData$savesByStrength$game$displayItems
#>  [1] "playerId"           "goalieFullName"     "gameId"            
#>  [4] "opponentTeamAbbrev" "gameDate"           "teamAbbrev"        
#>  [7] "shootsCatches"      "gamesPlayed"        "gamesStarted"      
#> [10] "wins"               "losses"             "ties"              
#> [13] "otLosses"           "shotsAgainst"       "saves"             
#> [16] "goalsAgainst"       "savePct"            "evShotsAgainst"    
#> [19] "evSaves"            "evGoalsAgainst"     "evSavePct"         
#> [22] "ppShotsAgainst"     "ppSaves"            "ppGoalsAgainst"    
#> [25] "ppSavePct"          "shShotsAgainst"     "shSaves"           
#> [28] "shGoalsAgainst"     "shSavePct"         
#> 
#> $goalieReportData$savesByStrength$game$resultFilters
#>  [1] "gamesPlayed"    "gamesStarted"   "wins"           "losses"        
#>  [5] "ties"           "otLosses"       "shotsAgainst"   "saves"         
#>  [9] "goalsAgainst"   "savePct"        "evShotsAgainst" "evSaves"       
#> [13] "evGoalsAgainst" "evSavePct"      "ppShotsAgainst" "ppSaves"       
#> [17] "ppGoalsAgainst" "ppSavePct"      "shShotsAgainst" "shSaves"       
#> [21] "shGoalsAgainst" "shSavePct"     
#> 
#> $goalieReportData$savesByStrength$game$sortKeys
#> [1] "wins"    "savePct"
#> 
#> 
#> $goalieReportData$savesByStrength$season
#> $goalieReportData$savesByStrength$season$displayItems
#>  [1] "playerId"       "goalieFullName" "seasonId"       "teamAbbrevs"   
#>  [5] "shootsCatches"  "gamesPlayed"    "gamesStarted"   "wins"          
#>  [9] "losses"         "ties"           "otLosses"       "shotsAgainst"  
#> [13] "saves"          "goalsAgainst"   "savePct"        "evShotsAgainst"
#> [17] "evSaves"        "evGoalsAgainst" "evSavePct"      "ppShotsAgainst"
#> [21] "ppSaves"        "ppGoalsAgainst" "ppSavePct"      "shShotsAgainst"
#> [25] "shSaves"        "shGoalsAgainst" "shSavePct"     
#> 
#> $goalieReportData$savesByStrength$season$resultFilters
#>  [1] "gamesPlayed"    "gamesStarted"   "wins"           "losses"        
#>  [5] "ties"           "otLosses"       "shotsAgainst"   "saves"         
#>  [9] "goalsAgainst"   "savePct"        "evShotsAgainst" "evSaves"       
#> [13] "evGoalsAgainst" "evSavePct"      "ppShotsAgainst" "ppSaves"       
#> [17] "ppGoalsAgainst" "ppSavePct"      "shShotsAgainst" "shSaves"       
#> [21] "shGoalsAgainst" "shSavePct"     
#> 
#> $goalieReportData$savesByStrength$season$sortKeys
#> [1] "wins"    "savePct"
#> 
#> 
#> 
#> $goalieReportData$daysrest
#> $goalieReportData$daysrest$game
#> $goalieReportData$daysrest$game$displayItems
#>  [1] "playerId"                 "goalieFullName"          
#>  [3] "gameId"                   "opponentTeamAbbrev"      
#>  [5] "gameDate"                 "teamAbbrev"              
#>  [7] "shootsCatches"            "gamesPlayed"             
#>  [9] "gamesStarted"             "wins"                    
#> [11] "losses"                   "ties"                    
#> [13] "otLosses"                 "savePct"                 
#> [15] "gamesPlayedDaysRest0"     "gamesPlayedDaysRest1"    
#> [17] "gamesPlayedDaysRest2"     "gamesPlayedDaysRest3"    
#> [19] "gamesPlayedDaysRest4Plus" "savePctDaysRest0"        
#> [21] "savePctDaysRest1"         "savePctDaysRest2"        
#> [23] "savePctDaysRest3"         "savePctDaysRest4Plus"    
#> 
#> $goalieReportData$daysrest$game$resultFilters
#>  [1] "gamesPlayed"              "gamesStarted"            
#>  [3] "wins"                     "losses"                  
#>  [5] "ties"                     "otLosses"                
#>  [7] "savePct"                  "gamesPlayedDaysRest0"    
#>  [9] "gamesPlayedDaysRest1"     "gamesPlayedDaysRest2"    
#> [11] "gamesPlayedDaysRest3"     "gamesPlayedDaysRest4Plus"
#> [13] "savePctDaysRest0"         "savePctDaysRest1"        
#> [15] "savePctDaysRest2"         "savePctDaysRest3"        
#> [17] "savePctDaysRest4Plus"    
#> 
#> $goalieReportData$daysrest$game$sortKeys
#> [1] "wins"    "savePct"
#> 
#> 
#> $goalieReportData$daysrest$season
#> $goalieReportData$daysrest$season$displayItems
#>  [1] "playerId"                 "goalieFullName"          
#>  [3] "seasonId"                 "teamAbbrevs"             
#>  [5] "shootsCatches"            "gamesPlayed"             
#>  [7] "gamesStarted"             "wins"                    
#>  [9] "losses"                   "ties"                    
#> [11] "otLosses"                 "savePct"                 
#> [13] "gamesPlayedDaysRest0"     "gamesPlayedDaysRest1"    
#> [15] "gamesPlayedDaysRest2"     "gamesPlayedDaysRest3"    
#> [17] "gamesPlayedDaysRest4Plus" "savePctDaysRest0"        
#> [19] "savePctDaysRest1"         "savePctDaysRest2"        
#> [21] "savePctDaysRest3"         "savePctDaysRest4Plus"    
#> 
#> $goalieReportData$daysrest$season$resultFilters
#>  [1] "gamesPlayed"              "gamesStarted"            
#>  [3] "wins"                     "losses"                  
#>  [5] "ties"                     "otLosses"                
#>  [7] "savePct"                  "gamesPlayedDaysRest0"    
#>  [9] "gamesPlayedDaysRest1"     "gamesPlayedDaysRest2"    
#> [11] "gamesPlayedDaysRest3"     "gamesPlayedDaysRest4Plus"
#> [13] "savePctDaysRest0"         "savePctDaysRest1"        
#> [15] "savePctDaysRest2"         "savePctDaysRest3"        
#> [17] "savePctDaysRest4Plus"    
#> 
#> $goalieReportData$daysrest$season$sortKeys
#> [1] "wins"    "savePct"
#> 
#> 
#> 
#> $goalieReportData$penaltyShots
#> $goalieReportData$penaltyShots$game
#> $goalieReportData$penaltyShots$game$displayItems
#>  [1] "playerId"                 "goalieFullName"          
#>  [3] "gameId"                   "opponentTeamAbbrev"      
#>  [5] "gameDate"                 "teamAbbrev"              
#>  [7] "shootsCatches"            "gamesPlayed"             
#>  [9] "shotsAgainst"             "saves"                   
#> [11] "goalsAgainst"             "savePct"                 
#> [13] "penaltyShotsAgainst"      "penaltyShotsSaves"       
#> [15] "penaltyShotsGoalsAgainst" "penaltyShotSavePct"      
#> 
#> $goalieReportData$penaltyShots$game$resultFilters
#> [1] "gamesPlayed"              "shotsAgainst"            
#> [3] "saves"                    "goalsAgainst"            
#> [5] "savePct"                  "penaltyShotsAgainst"     
#> [7] "penaltyShotsSaves"        "penaltyShotsGoalsAgainst"
#> [9] "penaltyShotSavePct"      
#> 
#> $goalieReportData$penaltyShots$game$sortKeys
#> [1] "penaltyShotsSaves"  "penaltyShotSavePct"
#> 
#> 
#> $goalieReportData$penaltyShots$season
#> $goalieReportData$penaltyShots$season$displayItems
#>  [1] "playerId"                 "goalieFullName"          
#>  [3] "seasonId"                 "teamAbbrevs"             
#>  [5] "shootsCatches"            "gamesPlayed"             
#>  [7] "shotsAgainst"             "saves"                   
#>  [9] "goalsAgainst"             "savePct"                 
#> [11] "penaltyShotsAgainst"      "penaltyShotsSaves"       
#> [13] "penaltyShotsGoalsAgainst" "penaltyShotSavePct"      
#> 
#> $goalieReportData$penaltyShots$season$resultFilters
#> [1] "gamesPlayed"              "shotsAgainst"            
#> [3] "saves"                    "goalsAgainst"            
#> [5] "savePct"                  "penaltyShotsAgainst"     
#> [7] "penaltyShotsSaves"        "penaltyShotsGoalsAgainst"
#> [9] "penaltyShotSavePct"      
#> 
#> $goalieReportData$penaltyShots$season$sortKeys
#> [1] "penaltyShotsSaves"  "penaltyShotSavePct"
#> 
#> 
#> 
#> 
#> $teamReportData
#> $teamReportData$summary
#> $teamReportData$summary$game
#> $teamReportData$summary$game$displayItems
#>  [1] "teamId"              "franchiseId"         "teamFullName"       
#>  [4] "franchiseName"       "gameId"              "opponentTeamAbbrev" 
#>  [7] "gameDate"            "gamesPlayed"         "wins"               
#> [10] "losses"              "ties"                "otLosses"           
#> [13] "points"              "pointPct"            "winsInRegulation"   
#> [16] "regulationAndOtWins" "winsInShootout"      "goalsFor"           
#> [19] "goalsAgainst"        "goalsForPerGame"     "goalsAgainstPerGame"
#> [22] "teamShutouts"        "powerPlayPct"        "penaltyKillPct"     
#> [25] "powerPlayNetPct"     "penaltyKillNetPct"   "shotsForPerGame"    
#> [28] "shotsAgainstPerGame" "faceoffWinPct"      
#> 
#> $teamReportData$summary$game$resultFilters
#>  [1] "gamesPlayed"         "wins"                "losses"             
#>  [4] "ties"                "otLosses"            "points"             
#>  [7] "pointPct"            "winsInRegulation"    "regulationAndOtWins"
#> [10] "winsInShootout"      "goalsFor"            "goalsAgainst"       
#> [13] "goalsForPerGame"     "goalsAgainstPerGame" "teamShutouts"       
#> [16] "powerPlayPct"        "penaltyKillPct"      "powerPlayNetPct"    
#> [19] "penaltyKillNetPct"   "shotsForPerGame"     "shotsAgainstPerGame"
#> [22] "faceoffWinPct"      
#> 
#> $teamReportData$summary$game$sortKeys
#> [1] "points" "wins"  
#> 
#> 
#> $teamReportData$summary$season
#> $teamReportData$summary$season$displayItems
#>  [1] "teamId"              "franchiseId"         "teamFullName"       
#>  [4] "franchiseName"       "seasonId"            "gamesPlayed"        
#>  [7] "wins"                "losses"              "ties"               
#> [10] "otLosses"            "points"              "pointPct"           
#> [13] "winsInRegulation"    "regulationAndOtWins" "winsInShootout"     
#> [16] "goalsFor"            "goalsAgainst"        "goalsForPerGame"    
#> [19] "goalsAgainstPerGame" "teamShutouts"        "powerPlayPct"       
#> [22] "penaltyKillPct"      "powerPlayNetPct"     "penaltyKillNetPct"  
#> [25] "shotsForPerGame"     "shotsAgainstPerGame" "faceoffWinPct"      
#> 
#> $teamReportData$summary$season$resultFilters
#>  [1] "gamesPlayed"         "wins"                "losses"             
#>  [4] "ties"                "otLosses"            "points"             
#>  [7] "pointPct"            "winsInRegulation"    "regulationAndOtWins"
#> [10] "winsInShootout"      "goalsFor"            "goalsAgainst"       
#> [13] "goalsForPerGame"     "goalsAgainstPerGame" "teamShutouts"       
#> [16] "powerPlayPct"        "penaltyKillPct"      "powerPlayNetPct"    
#> [19] "penaltyKillNetPct"   "shotsForPerGame"     "shotsAgainstPerGame"
#> [22] "faceoffWinPct"      
#> 
#> $teamReportData$summary$season$sortKeys
#> [1] "points" "wins"  
#> 
#> 
#> 
#> $teamReportData$goalsforbystrengthgoaliepull
#> $teamReportData$goalsforbystrengthgoaliepull$game
#> $teamReportData$goalsforbystrengthgoaliepull$game$displayItems
#>  [1] "teamId"             "franchiseId"        "teamFullName"      
#>  [4] "franchiseName"      "gameId"             "opponentTeamAbbrev"
#>  [7] "gameDate"           "gamesPlayed"        "wins"              
#> [10] "losses"             "ties"               "otLosses"          
#> [13] "points"             "pointPct"           "goalsForAllPulls"  
#> [16] "goalsAgainst"       "goalsFor6On5"       "goalsFor6On4"      
#> [19] "goalsFor6On3"       "goalsFor3On6"       "goalsFor4On6"      
#> [22] "goalsFor5On6"       "goalsFor5On4"       "goalsFor5On3"      
#> [25] "goalsFor4On5"       "goalsFor4On3"       "goalsFor3On4"      
#> [28] "goalsFor3On5"       "goalsFor6On6"       "goalsFor5On5"      
#> [31] "goalsFor4On4"       "goalsForPerGame"   
#> 
#> $teamReportData$goalsforbystrengthgoaliepull$game$resultFilters
#>  [1] "gamesPlayed"      "wins"             "losses"           "ties"            
#>  [5] "otLosses"         "points"           "pointPct"         "goalsForAllPulls"
#>  [9] "goalsAgainst"     "goalsFor6On5"     "goalsFor6On4"     "goalsFor6On3"    
#> [13] "goalsFor3On6"     "goalsFor4On6"     "goalsFor5On6"     "goalsFor5On4"    
#> [17] "goalsFor5On3"     "goalsFor4On5"     "goalsFor4On3"     "goalsFor3On4"    
#> [21] "goalsFor3On5"     "goalsFor6On6"     "goalsFor5On5"     "goalsFor4On4"    
#> [25] "goalsForPerGame" 
#> 
#> $teamReportData$goalsforbystrengthgoaliepull$game$sortKeys
#> list()
#> 
#> 
#> $teamReportData$goalsforbystrengthgoaliepull$season
#> $teamReportData$goalsforbystrengthgoaliepull$season$displayItems
#>  [1] "teamId"           "franchiseId"      "teamFullName"     "franchiseName"   
#>  [5] "seasonId"         "gamesPlayed"      "wins"             "losses"          
#>  [9] "ties"             "otLosses"         "points"           "pointPct"        
#> [13] "goalsForAllPulls" "goalsAgainst"     "goalsFor6On5"     "goalsFor6On4"    
#> [17] "goalsFor6On3"     "goalsFor3On6"     "goalsFor4On6"     "goalsFor5On6"    
#> [21] "goalsFor5On4"     "goalsFor5On3"     "goalsFor4On5"     "goalsFor4On3"    
#> [25] "goalsFor3On4"     "goalsFor3On5"     "goalsFor6On6"     "goalsFor5On5"    
#> [29] "goalsFor4On4"     "goalsForPerGame" 
#> 
#> $teamReportData$goalsforbystrengthgoaliepull$season$resultFilters
#>  [1] "gamesPlayed"      "wins"             "losses"           "ties"            
#>  [5] "otLosses"         "points"           "pointPct"         "goalsForAllPulls"
#>  [9] "goalsAgainst"     "goalsFor6On5"     "goalsFor6On4"     "goalsFor6On3"    
#> [13] "goalsFor3On6"     "goalsFor4On6"     "goalsFor5On6"     "goalsFor5On4"    
#> [17] "goalsFor5On3"     "goalsFor4On5"     "goalsFor4On3"     "goalsFor3On4"    
#> [21] "goalsFor3On5"     "goalsFor6On6"     "goalsFor5On5"     "goalsFor4On4"    
#> [25] "goalsForPerGame" 
#> 
#> $teamReportData$goalsforbystrengthgoaliepull$season$sortKeys
#> list()
#> 
#> 
#> 
#> $teamReportData$goalsagainstbystrength
#> $teamReportData$goalsagainstbystrength$game
#> $teamReportData$goalsagainstbystrength$game$displayItems
#>  [1] "teamId"                    "franchiseId"              
#>  [3] "teamFullName"              "franchiseName"            
#>  [5] "gameId"                    "opponentTeamAbbrev"       
#>  [7] "gameDate"                  "gamesPlayed"              
#>  [9] "wins"                      "losses"                   
#> [11] "ties"                      "otLosses"                 
#> [13] "points"                    "pointPct"                 
#> [15] "goalsFor"                  "goalsAgainst"             
#> [17] "goalsAgainst5On5"          "goalsAgainst4On4"         
#> [19] "goalsAgainst3On3"          "goalsAgainst5On4"         
#> [21] "goalsAgainst5On3"          "goalsAgainst4On3"         
#> [23] "goalsAgainst3On4"          "goalsAgainst3On5"         
#> [25] "goalsAgainst4On5"          "goalsAgainstPenaltyShots" 
#> [27] "goalsAgainstEmptyNet"      "goalsAgainstExtraAttacker"
#> [29] "goalsAgainstPerGame"      
#> 
#> $teamReportData$goalsagainstbystrength$game$resultFilters
#>  [1] "gamesPlayed"               "wins"                     
#>  [3] "losses"                    "ties"                     
#>  [5] "otLosses"                  "points"                   
#>  [7] "pointPct"                  "goalsFor"                 
#>  [9] "goalsAgainst"              "goalsAgainst5On5"         
#> [11] "goalsAgainst4On4"          "goalsAgainst3On3"         
#> [13] "goalsAgainst5On4"          "goalsAgainst5On3"         
#> [15] "goalsAgainst4On3"          "goalsAgainst3On4"         
#> [17] "goalsAgainst3On5"          "goalsAgainst4On5"         
#> [19] "goalsAgainstPenaltyShots"  "goalsAgainstEmptyNet"     
#> [21] "goalsAgainstExtraAttacker" "goalsAgainstPerGame"      
#> 
#> $teamReportData$goalsagainstbystrength$game$sortKeys
#> [1] "goalsAgainst"
#> 
#> 
#> $teamReportData$goalsagainstbystrength$season
#> $teamReportData$goalsagainstbystrength$season$displayItems
#>  [1] "teamId"                    "franchiseId"              
#>  [3] "teamFullName"              "franchiseName"            
#>  [5] "seasonId"                  "gamesPlayed"              
#>  [7] "wins"                      "losses"                   
#>  [9] "ties"                      "otLosses"                 
#> [11] "points"                    "pointPct"                 
#> [13] "goalsFor"                  "goalsAgainst"             
#> [15] "goalsAgainst5On5"          "goalsAgainst4On4"         
#> [17] "goalsAgainst3On3"          "goalsAgainst5On4"         
#> [19] "goalsAgainst5On3"          "goalsAgainst4On3"         
#> [21] "goalsAgainst3On4"          "goalsAgainst3On5"         
#> [23] "goalsAgainst4On5"          "goalsAgainstPenaltyShots" 
#> [25] "goalsAgainstEmptyNet"      "goalsAgainstExtraAttacker"
#> [27] "goalsAgainstPerGame"      
#> 
#> $teamReportData$goalsagainstbystrength$season$resultFilters
#>  [1] "gamesPlayed"               "wins"                     
#>  [3] "losses"                    "ties"                     
#>  [5] "otLosses"                  "points"                   
#>  [7] "pointPct"                  "goalsFor"                 
#>  [9] "goalsAgainst"              "goalsAgainst5On5"         
#> [11] "goalsAgainst4On4"          "goalsAgainst3On3"         
#> [13] "goalsAgainst5On4"          "goalsAgainst5On3"         
#> [15] "goalsAgainst4On3"          "goalsAgainst3On4"         
#> [17] "goalsAgainst3On5"          "goalsAgainst4On5"         
#> [19] "goalsAgainstPenaltyShots"  "goalsAgainstEmptyNet"     
#> [21] "goalsAgainstExtraAttacker" "goalsAgainstPerGame"      
#> 
#> $teamReportData$goalsagainstbystrength$season$sortKeys
#> [1] "goalsAgainst"
#> 
#> 
#> 
#> $teamReportData$realtime
#> $teamReportData$realtime$game
#> $teamReportData$realtime$game$displayItems
#>  [1] "teamId"              "franchiseId"         "teamFullName"       
#>  [4] "franchiseName"       "gameId"              "opponentTeamAbbrev" 
#>  [7] "gameDate"            "gamesPlayed"         "timeOnIcePerGame5v5"
#> [10] "satPct"              "hits"                "hitsPer60"          
#> [13] "blockedShots"        "blockedShotsPer60"   "giveaways"          
#> [16] "giveawaysPer60"      "takeaways"           "takeawaysPer60"     
#> [19] "emptyNetGoals"       "shots"               "missedShots"        
#> [22] "shotAttemptsBlocked" "totalShotAttempts"  
#> 
#> $teamReportData$realtime$game$resultFilters
#>  [1] "gamesPlayed"         "timeOnIcePerGame5v5" "satPct"             
#>  [4] "hits"                "hitsPer60"           "blockedShots"       
#>  [7] "blockedShotsPer60"   "giveaways"           "giveawaysPer60"     
#> [10] "takeaways"           "takeawaysPer60"      "emptyNetGoals"      
#> [13] "shots"               "missedShots"         "shotAttemptsBlocked"
#> [16] "totalShotAttempts"  
#> 
#> $teamReportData$realtime$game$sortKeys
#> [1] "hits"
#> 
#> 
#> $teamReportData$realtime$season
#> $teamReportData$realtime$season$displayItems
#>  [1] "teamId"              "franchiseId"         "teamFullName"       
#>  [4] "franchiseName"       "seasonId"            "gamesPlayed"        
#>  [7] "timeOnIcePerGame5v5" "satPct"              "hits"               
#> [10] "hitsPer60"           "blockedShots"        "blockedShotsPer60"  
#> [13] "giveaways"           "giveawaysPer60"      "takeaways"          
#> [16] "takeawaysPer60"      "emptyNetGoals"       "shots"              
#> [19] "missedShots"         "shotAttemptsBlocked" "totalShotAttempts"  
#> 
#> $teamReportData$realtime$season$resultFilters
#>  [1] "gamesPlayed"         "timeOnIcePerGame5v5" "satPct"             
#>  [4] "hits"                "hitsPer60"           "blockedShots"       
#>  [7] "blockedShotsPer60"   "giveaways"           "giveawaysPer60"     
#> [10] "takeaways"           "takeawaysPer60"      "emptyNetGoals"      
#> [13] "shots"               "missedShots"         "shotAttemptsBlocked"
#> [16] "totalShotAttempts"  
#> 
#> $teamReportData$realtime$season$sortKeys
#> [1] "hits"
#> 
#> 
#> 
#> $teamReportData$penaltykilltime
#> $teamReportData$penaltykilltime$game
#> $teamReportData$penaltykilltime$game$displayItems
#>  [1] "teamId"                  "franchiseId"            
#>  [3] "teamFullName"            "franchiseName"          
#>  [5] "gameId"                  "opponentTeamAbbrev"     
#>  [7] "gameDate"                "gamesPlayed"            
#>  [9] "pointPct"                "timeOnIceShorthanded"   
#> [11] "timesShorthanded"        "shorthandedGoalsAgainst"
#> [13] "overallPenaltyKillPct"   "timeOnIce4v5"           
#> [15] "timesShorthanded4v5"     "goalsAgainst4v5"        
#> [17] "penaltyKillPct4v5"       "timeOnIce3v5"           
#> [19] "timesShorthanded3v5"     "goalsAgainst3v5"        
#> [21] "penaltyKillPct3v5"       "timeOnIce3v4"           
#> [23] "timesShorthanded3v4"     "goalsAgainst3v4"        
#> [25] "penaltyKillPct3v4"      
#> 
#> $teamReportData$penaltykilltime$game$resultFilters
#>  [1] "gamesPlayed"             "pointPct"               
#>  [3] "timeOnIceShorthanded"    "timesShorthanded"       
#>  [5] "shorthandedGoalsAgainst" "overallPenaltyKillPct"  
#>  [7] "timeOnIce4v5"            "timesShorthanded4v5"    
#>  [9] "goalsAgainst4v5"         "penaltyKillPct4v5"      
#> [11] "timeOnIce3v5"            "timesShorthanded3v5"    
#> [13] "goalsAgainst3v5"         "penaltyKillPct3v5"      
#> [15] "timeOnIce3v4"            "timesShorthanded3v4"    
#> [17] "goalsAgainst3v4"         "penaltyKillPct3v4"      
#> 
#> $teamReportData$penaltykilltime$game$sortKeys
#> [1] "timeOnIceShorthanded"
#> 
#> 
#> $teamReportData$penaltykilltime$season
#> $teamReportData$penaltykilltime$season$displayItems
#>  [1] "teamId"                  "franchiseId"            
#>  [3] "teamFullName"            "franchiseName"          
#>  [5] "seasonId"                "gamesPlayed"            
#>  [7] "pointPct"                "timeOnIceShorthanded"   
#>  [9] "timesShorthanded"        "shorthandedGoalsAgainst"
#> [11] "overallPenaltyKillPct"   "timeOnIce4v5"           
#> [13] "timesShorthanded4v5"     "goalsAgainst4v5"        
#> [15] "penaltyKillPct4v5"       "timeOnIce3v5"           
#> [17] "timesShorthanded3v5"     "goalsAgainst3v5"        
#> [19] "penaltyKillPct3v5"       "timeOnIce3v4"           
#> [21] "timesShorthanded3v4"     "goalsAgainst3v4"        
#> [23] "penaltyKillPct3v4"      
#> 
#> $teamReportData$penaltykilltime$season$resultFilters
#>  [1] "gamesPlayed"             "pointPct"               
#>  [3] "timeOnIceShorthanded"    "timesShorthanded"       
#>  [5] "shorthandedGoalsAgainst" "overallPenaltyKillPct"  
#>  [7] "timeOnIce4v5"            "timesShorthanded4v5"    
#>  [9] "goalsAgainst4v5"         "penaltyKillPct4v5"      
#> [11] "timeOnIce3v5"            "timesShorthanded3v5"    
#> [13] "goalsAgainst3v5"         "penaltyKillPct3v5"      
#> [15] "timeOnIce3v4"            "timesShorthanded3v4"    
#> [17] "goalsAgainst3v4"         "penaltyKillPct3v4"      
#> 
#> $teamReportData$penaltykilltime$season$sortKeys
#> [1] "timeOnIceShorthanded"
#> 
#> 
#> 
#> $teamReportData$goalgames
#> $teamReportData$goalgames$game
#> $teamReportData$goalgames$game$displayItems
#>  [1] "teamId"               "franchiseId"          "teamFullName"        
#>  [4] "franchiseName"        "gameId"               "opponentTeamAbbrev"  
#>  [7] "gameDate"             "gamesPlayed"          "wins"                
#> [10] "losses"               "ties"                 "otLosses"            
#> [13] "points"               "pointPct"             "winPctOneGoalGames"  
#> [16] "winPctTwoGoalGames"   "winPctThreeGoalGames" "winsOneGoalGames"    
#> [19] "winsTwoGoalGames"     "winsThreeGoalGames"   "lossesOneGoalGames"  
#> [22] "lossesTwoGoalGames"   "lossesThreeGoalGames" "otLossesOneGoalGames"
#> 
#> $teamReportData$goalgames$game$resultFilters
#>  [1] "gamesPlayed"          "wins"                 "losses"              
#>  [4] "ties"                 "otLosses"             "points"              
#>  [7] "pointPct"             "winPctOneGoalGames"   "winPctTwoGoalGames"  
#> [10] "winPctThreeGoalGames" "winsOneGoalGames"     "winsTwoGoalGames"    
#> [13] "winsThreeGoalGames"   "lossesOneGoalGames"   "lossesTwoGoalGames"  
#> [16] "lossesThreeGoalGames" "otLossesOneGoalGames"
#> 
#> $teamReportData$goalgames$game$sortKeys
#> [1] "winPctOneGoalGames"
#> 
#> 
#> $teamReportData$goalgames$season
#> $teamReportData$goalgames$season$displayItems
#>  [1] "teamId"               "franchiseId"          "teamFullName"        
#>  [4] "franchiseName"        "seasonId"             "gamesPlayed"         
#>  [7] "wins"                 "losses"               "ties"                
#> [10] "otLosses"             "points"               "pointPct"            
#> [13] "winPctOneGoalGames"   "winPctTwoGoalGames"   "winPctThreeGoalGames"
#> [16] "winsOneGoalGames"     "winsTwoGoalGames"     "winsThreeGoalGames"  
#> [19] "lossesOneGoalGames"   "lossesTwoGoalGames"   "lossesThreeGoalGames"
#> [22] "otLossesOneGoalGames"
#> 
#> $teamReportData$goalgames$season$resultFilters
#>  [1] "gamesPlayed"          "wins"                 "losses"              
#>  [4] "ties"                 "otLosses"             "points"              
#>  [7] "pointPct"             "winPctOneGoalGames"   "winPctTwoGoalGames"  
#> [10] "winPctThreeGoalGames" "winsOneGoalGames"     "winsTwoGoalGames"    
#> [13] "winsThreeGoalGames"   "lossesOneGoalGames"   "lossesTwoGoalGames"  
#> [16] "lossesThreeGoalGames" "otLossesOneGoalGames"
#> 
#> $teamReportData$goalgames$season$sortKeys
#> [1] "winPctOneGoalGames"
#> 
#> 
#> 
#> $teamReportData$powerplaytime
#> $teamReportData$powerplaytime$game
#> $teamReportData$powerplaytime$game$displayItems
#>  [1] "teamId"              "franchiseId"         "teamFullName"       
#>  [4] "franchiseName"       "gameId"              "opponentTeamAbbrev" 
#>  [7] "gameDate"            "gamesPlayed"         "pointPct"           
#> [10] "timeOnIcePp"         "ppOpportunities"     "powerPlayGoalsFor"  
#> [13] "overallPowerPlayPct" "timeOnIce5v4"        "opportunities5v4"   
#> [16] "goals5v4"            "powerPlayPct5v4"     "timeOnIce5v3"       
#> [19] "opportunities5v3"    "goals5v3"            "powerPlayPct5v3"    
#> [22] "timeOnIce4v3"        "opportunities4v3"    "goals4v3"           
#> [25] "powerPlayPct4v3"    
#> 
#> $teamReportData$powerplaytime$game$resultFilters
#>  [1] "gamesPlayed"         "pointPct"            "timeOnIcePp"        
#>  [4] "ppOpportunities"     "powerPlayGoalsFor"   "overallPowerPlayPct"
#>  [7] "timeOnIce5v4"        "opportunities5v4"    "goals5v4"           
#> [10] "powerPlayPct5v4"     "timeOnIce5v3"        "opportunities5v3"   
#> [13] "goals5v3"            "powerPlayPct5v3"     "timeOnIce4v3"       
#> [16] "opportunities4v3"    "goals4v3"            "powerPlayPct4v3"    
#> 
#> $teamReportData$powerplaytime$game$sortKeys
#> [1] "timeOnIcePp"
#> 
#> 
#> $teamReportData$powerplaytime$season
#> $teamReportData$powerplaytime$season$displayItems
#>  [1] "teamId"              "franchiseId"         "teamFullName"       
#>  [4] "franchiseName"       "seasonId"            "gamesPlayed"        
#>  [7] "pointPct"            "timeOnIcePp"         "ppOpportunities"    
#> [10] "powerPlayGoalsFor"   "overallPowerPlayPct" "timeOnIce5v4"       
#> [13] "opportunities5v4"    "goals5v4"            "powerPlayPct5v4"    
#> [16] "timeOnIce5v3"        "opportunities5v3"    "goals5v3"           
#> [19] "powerPlayPct5v3"     "timeOnIce4v3"        "opportunities4v3"   
#> [22] "goals4v3"            "powerPlayPct4v3"    
#> 
#> $teamReportData$powerplaytime$season$resultFilters
#>  [1] "gamesPlayed"         "pointPct"            "timeOnIcePp"        
#>  [4] "ppOpportunities"     "powerPlayGoalsFor"   "overallPowerPlayPct"
#>  [7] "timeOnIce5v4"        "opportunities5v4"    "goals5v4"           
#> [10] "powerPlayPct5v4"     "timeOnIce5v3"        "opportunities5v3"   
#> [13] "goals5v3"            "powerPlayPct5v3"     "timeOnIce4v3"       
#> [16] "opportunities4v3"    "goals4v3"            "powerPlayPct4v3"    
#> 
#> $teamReportData$powerplaytime$season$sortKeys
#> [1] "timeOnIcePp"
#> 
#> 
#> 
#> $teamReportData$penalties
#> $teamReportData$penalties$game
#> $teamReportData$penalties$game$displayItems
#>  [1] "teamId"                "franchiseId"           "teamFullName"         
#>  [4] "franchiseName"         "gameId"                "opponentTeamAbbrev"   
#>  [7] "gameDate"              "gamesPlayed"           "wins"                 
#> [10] "losses"                "ties"                  "otLosses"             
#> [13] "points"                "penaltyMinutes"        "penaltySecondsPerGame"
#> [16] "totalPenaltiesDrawn"   "penalties"             "netPenalties"         
#> [19] "penaltiesDrawnPer60"   "penaltiesTakenPer60"   "netPenaltiesPer60"    
#> [22] "benchMinorPenalties"   "minors"                "majors"               
#> [25] "matchPenalties"        "misconducts"           "gameMisconducts"      
#> 
#> $teamReportData$penalties$game$resultFilters
#>  [1] "gamesPlayed"           "wins"                  "losses"               
#>  [4] "ties"                  "otLosses"              "points"               
#>  [7] "penaltyMinutes"        "penaltySecondsPerGame" "totalPenaltiesDrawn"  
#> [10] "penalties"             "netPenalties"          "penaltiesDrawnPer60"  
#> [13] "penaltiesTakenPer60"   "netPenaltiesPer60"     "benchMinorPenalties"  
#> [16] "minors"                "majors"                "matchPenalties"       
#> [19] "misconducts"           "gameMisconducts"      
#> 
#> $teamReportData$penalties$game$sortKeys
#> [1] "penaltyMinutes"
#> 
#> 
#> $teamReportData$penalties$season
#> $teamReportData$penalties$season$displayItems
#>  [1] "teamId"                "franchiseId"           "teamFullName"         
#>  [4] "franchiseName"         "seasonId"              "gamesPlayed"          
#>  [7] "wins"                  "losses"                "ties"                 
#> [10] "otLosses"              "points"                "penaltyMinutes"       
#> [13] "penaltySecondsPerGame" "totalPenaltiesDrawn"   "penalties"            
#> [16] "netPenalties"          "penaltiesDrawnPer60"   "penaltiesTakenPer60"  
#> [19] "netPenaltiesPer60"     "benchMinorPenalties"   "minors"               
#> [22] "majors"                "matchPenalties"        "misconducts"          
#> [25] "gameMisconducts"      
#> 
#> $teamReportData$penalties$season$resultFilters
#>  [1] "gamesPlayed"           "wins"                  "losses"               
#>  [4] "ties"                  "otLosses"              "points"               
#>  [7] "penaltyMinutes"        "penaltySecondsPerGame" "totalPenaltiesDrawn"  
#> [10] "penalties"             "netPenalties"          "penaltiesDrawnPer60"  
#> [13] "penaltiesTakenPer60"   "netPenaltiesPer60"     "benchMinorPenalties"  
#> [16] "minors"                "majors"                "matchPenalties"       
#> [19] "misconducts"           "gameMisconducts"      
#> 
#> $teamReportData$penalties$season$sortKeys
#> [1] "penaltyMinutes"
#> 
#> 
#> 
#> $teamReportData$shootout
#> $teamReportData$shootout$game
#> $teamReportData$shootout$game$displayItems
#>  [1] "teamId"                      "franchiseId"                
#>  [3] "teamFullName"                "franchiseName"              
#>  [5] "gameId"                      "opponentTeamAbbrev"         
#>  [7] "gameDate"                    "gamesPlayed"                
#>  [9] "wins"                        "losses"                     
#> [11] "ties"                        "otLosses"                   
#> [13] "points"                      "pointPct"                   
#> [15] "shootoutGamesPlayed"         "shootoutWins"               
#> [17] "shootoutLosses"              "shootoutPoints"             
#> [19] "shootoutWinPct"              "shootoutGoals"              
#> [21] "shootoutShots"               "shootoutShootingPct"        
#> [23] "shootoutShotsAgainst"        "shootoutGoalsAgainst"       
#> [25] "shootoutSaves"               "shootoutSavePct"            
#> [27] "shootoutShootingPlusSavePct"
#> 
#> $teamReportData$shootout$game$resultFilters
#>  [1] "gamesPlayed"                 "wins"                       
#>  [3] "losses"                      "ties"                       
#>  [5] "otLosses"                    "points"                     
#>  [7] "pointPct"                    "shootoutGamesPlayed"        
#>  [9] "shootoutWins"                "shootoutLosses"             
#> [11] "shootoutPoints"              "shootoutWinPct"             
#> [13] "shootoutGoals"               "shootoutShots"              
#> [15] "shootoutShootingPct"         "shootoutShotsAgainst"       
#> [17] "shootoutGoalsAgainst"        "shootoutSaves"              
#> [19] "shootoutSavePct"             "shootoutShootingPlusSavePct"
#> 
#> $teamReportData$shootout$game$sortKeys
#> [1] "shootoutWins"   "shootoutWinPct"
#> 
#> 
#> $teamReportData$shootout$season
#> $teamReportData$shootout$season$displayItems
#>  [1] "teamId"                      "franchiseId"                
#>  [3] "teamFullName"                "franchiseName"              
#>  [5] "seasonId"                    "gamesPlayed"                
#>  [7] "wins"                        "losses"                     
#>  [9] "ties"                        "otLosses"                   
#> [11] "points"                      "pointPct"                   
#> [13] "shootoutGamesPlayed"         "shootoutWins"               
#> [15] "shootoutLosses"              "shootoutPoints"             
#> [17] "shootoutWinPct"              "shootoutGoals"              
#> [19] "shootoutShots"               "shootoutShootingPct"        
#> [21] "shootoutShotsAgainst"        "shootoutGoalsAgainst"       
#> [23] "shootoutSaves"               "shootoutSavePct"            
#> [25] "shootoutShootingPlusSavePct"
#> 
#> $teamReportData$shootout$season$resultFilters
#>  [1] "gamesPlayed"                 "wins"                       
#>  [3] "losses"                      "ties"                       
#>  [5] "otLosses"                    "points"                     
#>  [7] "pointPct"                    "shootoutGamesPlayed"        
#>  [9] "shootoutWins"                "shootoutLosses"             
#> [11] "shootoutPoints"              "shootoutWinPct"             
#> [13] "shootoutGoals"               "shootoutShots"              
#> [15] "shootoutShootingPct"         "shootoutShotsAgainst"       
#> [17] "shootoutGoalsAgainst"        "shootoutSaves"              
#> [19] "shootoutSavePct"             "shootoutShootingPlusSavePct"
#> 
#> $teamReportData$shootout$season$sortKeys
#> [1] "shootoutWins"   "shootoutWinPct"
#> 
#> 
#> 
#> $teamReportData$shottype
#> $teamReportData$shottype$game
#> $teamReportData$shottype$game$displayItems
#>  [1] "teamId"                "franchiseId"           "teamFullName"         
#>  [4] "franchiseName"         "gameId"                "opponentTeamAbbrev"   
#>  [7] "gameDate"              "gamesPlayed"           "goalsFor"             
#> [10] "goalsWrist"            "goalsSnap"             "goalsSlap"            
#> [13] "goalsBackhand"         "goalsTipIn"            "goalsDeflected"       
#> [16] "goalsWrapAround"       "shotsOnNet"            "shotsOnNetWrist"      
#> [19] "shotsOnNetSnap"        "shotsOnNetSlap"        "shotsOnNetBackhand"   
#> [22] "shotsOnNetTipIn"       "shotsOnNetDeflected"   "shotsOnNetWrapAround" 
#> [25] "shootingPct"           "shootingPctWrist"      "shootingPctSnap"      
#> [28] "shootingPctSlap"       "shootingPctBackhand"   "shootingPctTipIn"     
#> [31] "shootingPctDeflected"  "shootingPctWrapAround"
#> 
#> $teamReportData$shottype$game$resultFilters
#>  [1] "gamesPlayed"           "goalsFor"              "goalsWrist"           
#>  [4] "goalsSnap"             "goalsSlap"             "goalsBackhand"        
#>  [7] "goalsTipIn"            "goalsDeflected"        "goalsWrapAround"      
#> [10] "shotsOnNet"            "shotsOnNetWrist"       "shotsOnNetSnap"       
#> [13] "shotsOnNetSlap"        "shotsOnNetBackhand"    "shotsOnNetTipIn"      
#> [16] "shotsOnNetDeflected"   "shotsOnNetWrapAround"  "shootingPct"          
#> [19] "shootingPctWrist"      "shootingPctSnap"       "shootingPctSlap"      
#> [22] "shootingPctBackhand"   "shootingPctTipIn"      "shootingPctDeflected" 
#> [25] "shootingPctWrapAround"
#> 
#> $teamReportData$shottype$game$sortKeys
#> [1] "shotsOnNet"  "shootingPct"
#> 
#> 
#> $teamReportData$shottype$season
#> $teamReportData$shottype$season$displayItems
#>  [1] "teamId"                "franchiseId"           "teamFullName"         
#>  [4] "franchiseName"         "seasonId"              "gamesPlayed"          
#>  [7] "goalsFor"              "goalsWrist"            "goalsSnap"            
#> [10] "goalsSlap"             "goalsBackhand"         "goalsTipIn"           
#> [13] "goalsDeflected"        "goalsWrapAround"       "shotsOnNet"           
#> [16] "shotsOnNetWrist"       "shotsOnNetSnap"        "shotsOnNetSlap"       
#> [19] "shotsOnNetBackhand"    "shotsOnNetTipIn"       "shotsOnNetDeflected"  
#> [22] "shotsOnNetWrapAround"  "shootingPct"           "shootingPctWrist"     
#> [25] "shootingPctSnap"       "shootingPctSlap"       "shootingPctBackhand"  
#> [28] "shootingPctTipIn"      "shootingPctDeflected"  "shootingPctWrapAround"
#> 
#> $teamReportData$shottype$season$resultFilters
#>  [1] "gamesPlayed"           "goalsFor"              "goalsWrist"           
#>  [4] "goalsSnap"             "goalsSlap"             "goalsBackhand"        
#>  [7] "goalsTipIn"            "goalsDeflected"        "goalsWrapAround"      
#> [10] "shotsOnNet"            "shotsOnNetWrist"       "shotsOnNetSnap"       
#> [13] "shotsOnNetSlap"        "shotsOnNetBackhand"    "shotsOnNetTipIn"      
#> [16] "shotsOnNetDeflected"   "shotsOnNetWrapAround"  "shootingPct"          
#> [19] "shootingPctWrist"      "shootingPctSnap"       "shootingPctSlap"      
#> [22] "shootingPctBackhand"   "shootingPctTipIn"      "shootingPctDeflected" 
#> [25] "shootingPctWrapAround"
#> 
#> $teamReportData$shottype$season$sortKeys
#> [1] "shotsOnNet"  "shootingPct"
#> 
#> 
#> 
#> $teamReportData$faceoffpercentages
#> $teamReportData$faceoffpercentages$game
#> $teamReportData$faceoffpercentages$game$displayItems
#>  [1] "teamId"                  "franchiseId"            
#>  [3] "teamFullName"            "franchiseName"          
#>  [5] "gameId"                  "opponentTeamAbbrev"     
#>  [7] "gameDate"                "gamesPlayed"            
#>  [9] "totalFaceoffs"           "evFaceoffs"             
#> [11] "ppFaceoffs"              "shFaceoffs"             
#> [13] "offensiveZoneFaceoffs"   "neutralZoneFaceoffs"    
#> [15] "defensiveZoneFaceoffs"   "faceoffWinPct"          
#> [17] "evFaceoffPct"            "ppFaceoffPct"           
#> [19] "shFaceoffPct"            "offensiveZoneFaceoffPct"
#> [21] "neutralZoneFaceoffPct"   "defensiveZoneFaceoffPct"
#> 
#> $teamReportData$faceoffpercentages$game$resultFilters
#>  [1] "gamesPlayed"             "totalFaceoffs"          
#>  [3] "evFaceoffs"              "ppFaceoffs"             
#>  [5] "shFaceoffs"              "offensiveZoneFaceoffs"  
#>  [7] "neutralZoneFaceoffs"     "defensiveZoneFaceoffs"  
#>  [9] "faceoffWinPct"           "evFaceoffPct"           
#> [11] "ppFaceoffPct"            "shFaceoffPct"           
#> [13] "offensiveZoneFaceoffPct" "neutralZoneFaceoffPct"  
#> [15] "defensiveZoneFaceoffPct"
#> 
#> $teamReportData$faceoffpercentages$game$sortKeys
#> [1] "faceoffWinPct"
#> 
#> 
#> $teamReportData$faceoffpercentages$season
#> $teamReportData$faceoffpercentages$season$displayItems
#>  [1] "teamId"                  "franchiseId"            
#>  [3] "teamFullName"            "franchiseName"          
#>  [5] "seasonId"                "gamesPlayed"            
#>  [7] "totalFaceoffs"           "evFaceoffs"             
#>  [9] "ppFaceoffs"              "shFaceoffs"             
#> [11] "offensiveZoneFaceoffs"   "neutralZoneFaceoffs"    
#> [13] "defensiveZoneFaceoffs"   "faceoffWinPct"          
#> [15] "evFaceoffPct"            "ppFaceoffPct"           
#> [17] "shFaceoffPct"            "offensiveZoneFaceoffPct"
#> [19] "neutralZoneFaceoffPct"   "defensiveZoneFaceoffPct"
#> 
#> $teamReportData$faceoffpercentages$season$resultFilters
#>  [1] "gamesPlayed"             "totalFaceoffs"          
#>  [3] "evFaceoffs"              "ppFaceoffs"             
#>  [5] "shFaceoffs"              "offensiveZoneFaceoffs"  
#>  [7] "neutralZoneFaceoffs"     "defensiveZoneFaceoffs"  
#>  [9] "faceoffWinPct"           "evFaceoffPct"           
#> [11] "ppFaceoffPct"            "shFaceoffPct"           
#> [13] "offensiveZoneFaceoffPct" "neutralZoneFaceoffPct"  
#> [15] "defensiveZoneFaceoffPct"
#> 
#> $teamReportData$faceoffpercentages$season$sortKeys
#> [1] "faceoffWinPct"
#> 
#> 
#> 
#> $teamReportData$percentages
#> $teamReportData$percentages$game
#> $teamReportData$percentages$game$displayItems
#>  [1] "teamId"                 "franchiseId"            "teamFullName"          
#>  [4] "franchiseName"          "gameId"                 "opponentTeamAbbrev"    
#>  [7] "gameDate"               "gamesPlayed"            "points"                
#> [10] "pointPct"               "goalsForPct"            "satPct"                
#> [13] "satPctAhead"            "satPctTied"             "satPctBehind"          
#> [16] "satPctClose"            "usatPct"                "usatPctAhead"          
#> [19] "usatPctTied"            "usatPctBehind"          "usatPctClose"          
#> [22] "zoneStartPct5v5"        "shootingPct5v5"         "savePct5v5"            
#> [25] "shootingPlusSavePct5v5"
#> 
#> $teamReportData$percentages$game$resultFilters
#>  [1] "gamesPlayed"            "points"                 "pointPct"              
#>  [4] "goalsForPct"            "satPct"                 "satPctAhead"           
#>  [7] "satPctTied"             "satPctBehind"           "satPctClose"           
#> [10] "usatPct"                "usatPctAhead"           "usatPctTied"           
#> [13] "usatPctBehind"          "usatPctClose"           "zoneStartPct5v5"       
#> [16] "shootingPct5v5"         "savePct5v5"             "shootingPlusSavePct5v5"
#> 
#> $teamReportData$percentages$game$sortKeys
#> [1] "satPct"
#> 
#> 
#> $teamReportData$percentages$season
#> $teamReportData$percentages$season$displayItems
#>  [1] "teamId"                 "franchiseId"            "teamFullName"          
#>  [4] "franchiseName"          "seasonId"               "gamesPlayed"           
#>  [7] "points"                 "pointPct"               "goalsForPct"           
#> [10] "satPct"                 "satPctAhead"            "satPctTied"            
#> [13] "satPctBehind"           "satPctClose"            "usatPct"               
#> [16] "usatPctAhead"           "usatPctTied"            "usatPctBehind"         
#> [19] "usatPctClose"           "zoneStartPct5v5"        "shootingPct5v5"        
#> [22] "savePct5v5"             "shootingPlusSavePct5v5"
#> 
#> $teamReportData$percentages$season$resultFilters
#>  [1] "gamesPlayed"            "points"                 "pointPct"              
#>  [4] "goalsForPct"            "satPct"                 "satPctAhead"           
#>  [7] "satPctTied"             "satPctBehind"           "satPctClose"           
#> [10] "usatPct"                "usatPctAhead"           "usatPctTied"           
#> [13] "usatPctBehind"          "usatPctClose"           "zoneStartPct5v5"       
#> [16] "shootingPct5v5"         "savePct5v5"             "shootingPlusSavePct5v5"
#> 
#> $teamReportData$percentages$season$sortKeys
#> [1] "satPct"
#> 
#> 
#> 
#> $teamReportData$scoretrailfirst
#> $teamReportData$scoretrailfirst$game
#> $teamReportData$scoretrailfirst$game$displayItems
#>  [1] "teamId"                   "franchiseId"             
#>  [3] "teamFullName"             "franchiseName"           
#>  [5] "gameId"                   "opponentTeamAbbrev"      
#>  [7] "gameDate"                 "gamesPlayed"             
#>  [9] "wins"                     "losses"                  
#> [11] "ties"                     "otLosses"                
#> [13] "points"                   "scoringFirstGamesPlayed" 
#> [15] "winsScoringFirst"         "lossesScoringFirst"      
#> [17] "tiesScoringFirst"         "otLossesScoringFirst"    
#> [19] "winPctScoringFirst"       "trailingFirstGamesPlayed"
#> [21] "winsTrailingFirst"        "lossesTrailingFirst"     
#> [23] "tiesTrailingFirst"        "otLossesTrailingFirst"   
#> [25] "winPctTrailingFirst"     
#> 
#> $teamReportData$scoretrailfirst$game$resultFilters
#>  [1] "gamesPlayed"              "wins"                    
#>  [3] "losses"                   "ties"                    
#>  [5] "otLosses"                 "points"                  
#>  [7] "scoringFirstGamesPlayed"  "winsScoringFirst"        
#>  [9] "lossesScoringFirst"       "tiesScoringFirst"        
#> [11] "otLossesScoringFirst"     "winPctScoringFirst"      
#> [13] "trailingFirstGamesPlayed" "winsTrailingFirst"       
#> [15] "lossesTrailingFirst"      "tiesTrailingFirst"       
#> [17] "otLossesTrailingFirst"    "winPctTrailingFirst"     
#> 
#> $teamReportData$scoretrailfirst$game$sortKeys
#> [1] "winsScoringFirst"
#> 
#> 
#> $teamReportData$scoretrailfirst$season
#> $teamReportData$scoretrailfirst$season$displayItems
#>  [1] "teamId"                   "franchiseId"             
#>  [3] "teamFullName"             "franchiseName"           
#>  [5] "seasonId"                 "gamesPlayed"             
#>  [7] "wins"                     "losses"                  
#>  [9] "ties"                     "otLosses"                
#> [11] "points"                   "scoringFirstGamesPlayed" 
#> [13] "winsScoringFirst"         "lossesScoringFirst"      
#> [15] "tiesScoringFirst"         "otLossesScoringFirst"    
#> [17] "winPctScoringFirst"       "trailingFirstGamesPlayed"
#> [19] "winsTrailingFirst"        "lossesTrailingFirst"     
#> [21] "tiesTrailingFirst"        "otLossesTrailingFirst"   
#> [23] "winPctTrailingFirst"     
#> 
#> $teamReportData$scoretrailfirst$season$resultFilters
#>  [1] "gamesPlayed"              "wins"                    
#>  [3] "losses"                   "ties"                    
#>  [5] "otLosses"                 "points"                  
#>  [7] "scoringFirstGamesPlayed"  "winsScoringFirst"        
#>  [9] "lossesScoringFirst"       "tiesScoringFirst"        
#> [11] "otLossesScoringFirst"     "winPctScoringFirst"      
#> [13] "trailingFirstGamesPlayed" "winsTrailingFirst"       
#> [15] "lossesTrailingFirst"      "tiesTrailingFirst"       
#> [17] "otLossesTrailingFirst"    "winPctTrailingFirst"     
#> 
#> $teamReportData$scoretrailfirst$season$sortKeys
#> [1] "winsScoringFirst"
#> 
#> 
#> 
#> $teamReportData$daysbetweengames
#> $teamReportData$daysbetweengames$game
#> $teamReportData$daysbetweengames$game$displayItems
#>  [1] "teamId"                  "franchiseId"            
#>  [3] "teamFullName"            "franchiseName"          
#>  [5] "gameId"                  "opponentTeamAbbrev"     
#>  [7] "gameDate"                "daysRest"               
#>  [9] "gamesPlayed"             "wins"                   
#> [11] "losses"                  "ties"                   
#> [13] "otLosses"                "points"                 
#> [15] "pointPct"                "goalsForPerGame"        
#> [17] "goalsAgainstPerGame"     "netGoalsPerGame"        
#> [19] "shotsForPerGame"         "shotsAgainstPerGame"    
#> [21] "shotDifferentialPerGame" "ppOpportunitiesPerGame" 
#> [23] "timesShorthandedPerGame" "powerPlayPct"           
#> [25] "penaltyKillPct"          "faceoffWinPct"          
#> 
#> $teamReportData$daysbetweengames$game$resultFilters
#>  [1] "daysRest"                "gamesPlayed"            
#>  [3] "wins"                    "losses"                 
#>  [5] "ties"                    "otLosses"               
#>  [7] "points"                  "pointPct"               
#>  [9] "goalsForPerGame"         "goalsAgainstPerGame"    
#> [11] "netGoalsPerGame"         "shotsForPerGame"        
#> [13] "shotsAgainstPerGame"     "shotDifferentialPerGame"
#> [15] "ppOpportunitiesPerGame"  "timesShorthandedPerGame"
#> [17] "powerPlayPct"            "penaltyKillPct"         
#> [19] "faceoffWinPct"          
#> 
#> $teamReportData$daysbetweengames$game$sortKeys
#> [1] "teamFullName" "daysRest"    
#> 
#> 
#> $teamReportData$daysbetweengames$season
#> $teamReportData$daysbetweengames$season$displayItems
#>  [1] "teamId"                  "franchiseId"            
#>  [3] "teamFullName"            "franchiseName"          
#>  [5] "seasonId"                "daysRest"               
#>  [7] "gamesPlayed"             "wins"                   
#>  [9] "losses"                  "ties"                   
#> [11] "otLosses"                "points"                 
#> [13] "pointPct"                "goalsForPerGame"        
#> [15] "goalsAgainstPerGame"     "netGoalsPerGame"        
#> [17] "shotsForPerGame"         "shotsAgainstPerGame"    
#> [19] "shotDifferentialPerGame" "ppOpportunitiesPerGame" 
#> [21] "timesShorthandedPerGame" "powerPlayPct"           
#> [23] "penaltyKillPct"          "faceoffWinPct"          
#> 
#> $teamReportData$daysbetweengames$season$resultFilters
#>  [1] "daysRest"                "gamesPlayed"            
#>  [3] "wins"                    "losses"                 
#>  [5] "ties"                    "otLosses"               
#>  [7] "points"                  "pointPct"               
#>  [9] "goalsForPerGame"         "goalsAgainstPerGame"    
#> [11] "netGoalsPerGame"         "shotsForPerGame"        
#> [13] "shotsAgainstPerGame"     "shotDifferentialPerGame"
#> [15] "ppOpportunitiesPerGame"  "timesShorthandedPerGame"
#> [17] "powerPlayPct"            "penaltyKillPct"         
#> [19] "faceoffWinPct"          
#> 
#> $teamReportData$daysbetweengames$season$sortKeys
#> [1] "teamFullName" "daysRest"    
#> 
#> 
#> 
#> $teamReportData$outshootoutshotby
#> $teamReportData$outshootoutshotby$game
#> $teamReportData$outshootoutshotby$game$displayItems
#>  [1] "teamId"                    "franchiseId"              
#>  [3] "teamFullName"              "franchiseName"            
#>  [5] "gameId"                    "opponentTeamAbbrev"       
#>  [7] "gameDate"                  "gamesPlayed"              
#>  [9] "wins"                      "losses"                   
#> [11] "ties"                      "otLosses"                 
#> [13] "points"                    "pointPct"                 
#> [15] "shotsForPerGame"           "shotsAgainstPerGame"      
#> [17] "netShotsPerGame"           "winsOutshootOpponent"     
#> [19] "lossesOutshootOpponent"    "tiesOutshootOpponent"     
#> [21] "otLossesOutshootOpponent"  "winsOutshotByOpponent"    
#> [23] "lossesOutshotByOpponent"   "tiesOutshotByOpponent"    
#> [25] "otLossesOutshotByOpponent" "winsEvenShots"            
#> [27] "lossesEvenShots"           "tiesEvenShots"            
#> [29] "otLossesEvenShots"        
#> 
#> $teamReportData$outshootoutshotby$game$resultFilters
#>  [1] "gamesPlayed"               "wins"                     
#>  [3] "losses"                    "ties"                     
#>  [5] "otLosses"                  "points"                   
#>  [7] "pointPct"                  "shotsForPerGame"          
#>  [9] "shotsAgainstPerGame"       "netShotsPerGame"          
#> [11] "winsOutshootOpponent"      "lossesOutshootOpponent"   
#> [13] "tiesOutshootOpponent"      "otLossesOutshootOpponent" 
#> [15] "winsOutshotByOpponent"     "lossesOutshotByOpponent"  
#> [17] "tiesOutshotByOpponent"     "otLossesOutshotByOpponent"
#> [19] "winsEvenShots"             "lossesEvenShots"          
#> [21] "tiesEvenShots"             "otLossesEvenShots"        
#> 
#> $teamReportData$outshootoutshotby$game$sortKeys
#> [1] "winsOutshootOpponent"
#> 
#> 
#> $teamReportData$outshootoutshotby$season
#> $teamReportData$outshootoutshotby$season$displayItems
#>  [1] "teamId"                    "franchiseId"              
#>  [3] "teamFullName"              "franchiseName"            
#>  [5] "seasonId"                  "gamesPlayed"              
#>  [7] "wins"                      "losses"                   
#>  [9] "ties"                      "otLosses"                 
#> [11] "points"                    "pointPct"                 
#> [13] "shotsForPerGame"           "shotsAgainstPerGame"      
#> [15] "netShotsPerGame"           "winsOutshootOpponent"     
#> [17] "lossesOutshootOpponent"    "tiesOutshootOpponent"     
#> [19] "otLossesOutshootOpponent"  "winsOutshotByOpponent"    
#> [21] "lossesOutshotByOpponent"   "tiesOutshotByOpponent"    
#> [23] "otLossesOutshotByOpponent" "winsEvenShots"            
#> [25] "lossesEvenShots"           "tiesEvenShots"            
#> [27] "otLossesEvenShots"        
#> 
#> $teamReportData$outshootoutshotby$season$resultFilters
#>  [1] "gamesPlayed"               "wins"                     
#>  [3] "losses"                    "ties"                     
#>  [5] "otLosses"                  "points"                   
#>  [7] "pointPct"                  "shotsForPerGame"          
#>  [9] "shotsAgainstPerGame"       "netShotsPerGame"          
#> [11] "winsOutshootOpponent"      "lossesOutshootOpponent"   
#> [13] "tiesOutshootOpponent"      "otLossesOutshootOpponent" 
#> [15] "winsOutshotByOpponent"     "lossesOutshotByOpponent"  
#> [17] "tiesOutshotByOpponent"     "otLossesOutshotByOpponent"
#> [19] "winsEvenShots"             "lossesEvenShots"          
#> [21] "tiesEvenShots"             "otLossesEvenShots"        
#> 
#> $teamReportData$outshootoutshotby$season$sortKeys
#> [1] "winsOutshootOpponent"
#> 
#> 
#> 
#> $teamReportData$leadingtrailing
#> $teamReportData$leadingtrailing$game
#> $teamReportData$leadingtrailing$game$displayItems
#>  [1] "teamId"              "franchiseId"         "teamFullName"       
#>  [4] "franchiseName"       "gameId"              "opponentTeamAbbrev" 
#>  [7] "gameDate"            "gamesPlayed"         "pointPct"           
#> [10] "period1GoalsFor"     "period1GoalsAgainst" "period2GoalsFor"    
#> [13] "period2GoalsAgainst" "winsLeadPeriod1"     "lossLeadPeriod1"    
#> [16] "tiesLeadPeriod1"     "otLossLeadPeriod1"   "winPctLeadPeriod1"  
#> [19] "winsLeadPeriod2"     "lossLeadPeriod2"     "tiesLeadPeriod2"    
#> [22] "otLossLeadPeriod2"   "winPctLeadPeriod2"   "winsTrailPeriod1"   
#> [25] "lossTrailPeriod1"    "tiesTrailPeriod1"    "otLossTrailPeriod1" 
#> [28] "winPctTrailPeriod1"  "winsTrailPeriod2"    "lossTrailPeriod2"   
#> [31] "tiesTrailPeriod2"    "otLossTrailPeriod2"  "winPctTrailPeriod2" 
#> 
#> $teamReportData$leadingtrailing$game$resultFilters
#>  [1] "gamesPlayed"         "pointPct"            "period1GoalsFor"    
#>  [4] "period1GoalsAgainst" "period2GoalsFor"     "period2GoalsAgainst"
#>  [7] "winsLeadPeriod1"     "lossLeadPeriod1"     "tiesLeadPeriod1"    
#> [10] "otLossLeadPeriod1"   "winPctLeadPeriod1"   "winsLeadPeriod2"    
#> [13] "lossLeadPeriod2"     "tiesLeadPeriod2"     "otLossLeadPeriod2"  
#> [16] "winPctLeadPeriod2"   "winsTrailPeriod1"    "lossTrailPeriod1"   
#> [19] "tiesTrailPeriod1"    "otLossTrailPeriod1"  "winPctTrailPeriod1" 
#> [22] "winsTrailPeriod2"    "lossTrailPeriod2"    "tiesTrailPeriod2"   
#> [25] "otLossTrailPeriod2"  "winPctTrailPeriod2" 
#> 
#> $teamReportData$leadingtrailing$game$sortKeys
#> [1] "winsLeadPeriod1"
#> 
#> 
#> $teamReportData$leadingtrailing$season
#> $teamReportData$leadingtrailing$season$displayItems
#>  [1] "teamId"              "franchiseId"         "teamFullName"       
#>  [4] "franchiseName"       "seasonId"            "gamesPlayed"        
#>  [7] "pointPct"            "period1GoalsFor"     "period1GoalsAgainst"
#> [10] "period2GoalsFor"     "period2GoalsAgainst" "winsLeadPeriod1"    
#> [13] "lossLeadPeriod1"     "tiesLeadPeriod1"     "otLossLeadPeriod1"  
#> [16] "winPctLeadPeriod1"   "winsLeadPeriod2"     "lossLeadPeriod2"    
#> [19] "tiesLeadPeriod2"     "otLossLeadPeriod2"   "winPctLeadPeriod2"  
#> [22] "winsTrailPeriod1"    "lossTrailPeriod1"    "tiesTrailPeriod1"   
#> [25] "otLossTrailPeriod1"  "winPctTrailPeriod1"  "winsTrailPeriod2"   
#> [28] "lossTrailPeriod2"    "tiesTrailPeriod2"    "otLossTrailPeriod2" 
#> [31] "winPctTrailPeriod2" 
#> 
#> $teamReportData$leadingtrailing$season$resultFilters
#>  [1] "gamesPlayed"         "pointPct"            "period1GoalsFor"    
#>  [4] "period1GoalsAgainst" "period2GoalsFor"     "period2GoalsAgainst"
#>  [7] "winsLeadPeriod1"     "lossLeadPeriod1"     "tiesLeadPeriod1"    
#> [10] "otLossLeadPeriod1"   "winPctLeadPeriod1"   "winsLeadPeriod2"    
#> [13] "lossLeadPeriod2"     "tiesLeadPeriod2"     "otLossLeadPeriod2"  
#> [16] "winPctLeadPeriod2"   "winsTrailPeriod1"    "lossTrailPeriod1"   
#> [19] "tiesTrailPeriod1"    "otLossTrailPeriod1"  "winPctTrailPeriod1" 
#> [22] "winsTrailPeriod2"    "lossTrailPeriod2"    "tiesTrailPeriod2"   
#> [25] "otLossTrailPeriod2"  "winPctTrailPeriod2" 
#> 
#> $teamReportData$leadingtrailing$season$sortKeys
#> [1] "winsLeadPeriod1"
#> 
#> 
#> 
#> $teamReportData$goalsbyperiod
#> $teamReportData$goalsbyperiod$game
#> $teamReportData$goalsbyperiod$game$displayItems
#>  [1] "teamId"               "franchiseId"          "teamFullName"        
#>  [4] "franchiseName"        "gameId"               "opponentTeamAbbrev"  
#>  [7] "gameDate"             "gamesPlayed"          "wins"                
#> [10] "losses"               "ties"                 "otLosses"            
#> [13] "points"               "pointPct"             "evGoalsFor"          
#> [16] "ppGoalsFor"           "shGoalsFor"           "goalsFor"            
#> [19] "period1GoalsFor"      "period2GoalsFor"      "period3GoalsFor"     
#> [22] "periodOtGoalsFor"     "goalsAgainst"         "period1GoalsAgainst" 
#> [25] "period2GoalsAgainst"  "period3GoalsAgainst"  "periodOtGoalsAgainst"
#> 
#> $teamReportData$goalsbyperiod$game$resultFilters
#>  [1] "gamesPlayed"          "wins"                 "losses"              
#>  [4] "ties"                 "otLosses"             "points"              
#>  [7] "pointPct"             "evGoalsFor"           "ppGoalsFor"          
#> [10] "shGoalsFor"           "goalsFor"             "period1GoalsFor"     
#> [13] "period2GoalsFor"      "period3GoalsFor"      "periodOtGoalsFor"    
#> [16] "goalsAgainst"         "period1GoalsAgainst"  "period2GoalsAgainst" 
#> [19] "period3GoalsAgainst"  "periodOtGoalsAgainst"
#> 
#> $teamReportData$goalsbyperiod$game$sortKeys
#> [1] "period1GoalsFor"
#> 
#> 
#> $teamReportData$goalsbyperiod$season
#> $teamReportData$goalsbyperiod$season$displayItems
#>  [1] "teamId"               "franchiseId"          "teamFullName"        
#>  [4] "franchiseName"        "seasonId"             "gamesPlayed"         
#>  [7] "wins"                 "losses"               "ties"                
#> [10] "otLosses"             "points"               "pointPct"            
#> [13] "evGoalsFor"           "ppGoalsFor"           "shGoalsFor"          
#> [16] "goalsFor"             "period1GoalsFor"      "period2GoalsFor"     
#> [19] "period3GoalsFor"      "periodOtGoalsFor"     "goalsAgainst"        
#> [22] "period1GoalsAgainst"  "period2GoalsAgainst"  "period3GoalsAgainst" 
#> [25] "periodOtGoalsAgainst"
#> 
#> $teamReportData$goalsbyperiod$season$resultFilters
#>  [1] "gamesPlayed"          "wins"                 "losses"              
#>  [4] "ties"                 "otLosses"             "points"              
#>  [7] "pointPct"             "evGoalsFor"           "ppGoalsFor"          
#> [10] "shGoalsFor"           "goalsFor"             "period1GoalsFor"     
#> [13] "period2GoalsFor"      "period3GoalsFor"      "periodOtGoalsFor"    
#> [16] "goalsAgainst"         "period1GoalsAgainst"  "period2GoalsAgainst" 
#> [19] "period3GoalsAgainst"  "periodOtGoalsAgainst"
#> 
#> $teamReportData$goalsbyperiod$season$sortKeys
#> [1] "period1GoalsFor"
#> 
#> 
#> 
#> $teamReportData$summaryshooting
#> $teamReportData$summaryshooting$game
#> $teamReportData$summaryshooting$game$displayItems
#>  [1] "teamId"             "franchiseId"        "teamFullName"      
#>  [4] "franchiseName"      "gameId"             "opponentTeamAbbrev"
#>  [7] "gameDate"           "gamesPlayed"        "shots5v5"          
#> [10] "satFor"             "satAgainst"         "satTotal"          
#> [13] "satTied"            "satAhead"           "satBehind"         
#> [16] "satClose"           "usatFor"            "usatAgainst"       
#> [19] "usatTotal"          "usatTied"           "usatAhead"         
#> [22] "usatBehind"         "usatClose"         
#> 
#> $teamReportData$summaryshooting$game$resultFilters
#>  [1] "gamesPlayed" "shots5v5"    "satFor"      "satAgainst"  "satTotal"   
#>  [6] "satTied"     "satAhead"    "satBehind"   "satClose"    "usatFor"    
#> [11] "usatAgainst" "usatTotal"   "usatTied"    "usatAhead"   "usatBehind" 
#> [16] "usatClose"  
#> 
#> $teamReportData$summaryshooting$game$sortKeys
#> [1] "satTotal"
#> 
#> 
#> $teamReportData$summaryshooting$season
#> $teamReportData$summaryshooting$season$displayItems
#>  [1] "teamId"        "franchiseId"   "teamFullName"  "franchiseName"
#>  [5] "seasonId"      "gamesPlayed"   "shots5v5"      "satFor"       
#>  [9] "satAgainst"    "satTotal"      "satTied"       "satAhead"     
#> [13] "satBehind"     "satClose"      "usatFor"       "usatAgainst"  
#> [17] "usatTotal"     "usatTied"      "usatAhead"     "usatBehind"   
#> [21] "usatClose"    
#> 
#> $teamReportData$summaryshooting$season$resultFilters
#>  [1] "gamesPlayed" "shots5v5"    "satFor"      "satAgainst"  "satTotal"   
#>  [6] "satTied"     "satAhead"    "satBehind"   "satClose"    "usatFor"    
#> [11] "usatAgainst" "usatTotal"   "usatTied"    "usatAhead"   "usatBehind" 
#> [16] "usatClose"  
#> 
#> $teamReportData$summaryshooting$season$sortKeys
#> [1] "satTotal"
#> 
#> 
#> 
#> $teamReportData$powerplay
#> $teamReportData$powerplay$game
#> $teamReportData$powerplay$game$displayItems
#>  [1] "teamId"                 "franchiseId"            "teamFullName"          
#>  [4] "franchiseName"          "gameId"                 "opponentTeamAbbrev"    
#>  [7] "gameDate"               "gamesPlayed"            "wins"                  
#> [10] "losses"                 "ties"                   "otLosses"              
#> [13] "points"                 "pointPct"               "ppOpportunities"       
#> [16] "powerPlayGoalsFor"      "shGoalsAgainst"         "ppNetGoals"            
#> [19] "ppTimeOnIcePerGame"     "ppOpportunitiesPerGame" "ppGoalsPerGame"        
#> [22] "shGoalsAgainstPerGame"  "ppNetGoalsPerGame"      "powerPlayPct"          
#> [25] "powerPlayNetPct"       
#> 
#> $teamReportData$powerplay$game$resultFilters
#>  [1] "gamesPlayed"            "wins"                   "losses"                
#>  [4] "ties"                   "otLosses"               "points"                
#>  [7] "pointPct"               "ppOpportunities"        "powerPlayGoalsFor"     
#> [10] "shGoalsAgainst"         "ppNetGoals"             "ppTimeOnIcePerGame"    
#> [13] "ppOpportunitiesPerGame" "ppGoalsPerGame"         "shGoalsAgainstPerGame" 
#> [16] "ppNetGoalsPerGame"      "powerPlayPct"           "powerPlayNetPct"       
#> 
#> $teamReportData$powerplay$game$sortKeys
#> [1] "powerPlayPct"
#> 
#> 
#> $teamReportData$powerplay$season
#> $teamReportData$powerplay$season$displayItems
#>  [1] "teamId"                 "franchiseId"            "teamFullName"          
#>  [4] "franchiseName"          "seasonId"               "gamesPlayed"           
#>  [7] "wins"                   "losses"                 "ties"                  
#> [10] "otLosses"               "points"                 "pointPct"              
#> [13] "ppOpportunities"        "powerPlayGoalsFor"      "shGoalsAgainst"        
#> [16] "ppNetGoals"             "ppTimeOnIcePerGame"     "ppOpportunitiesPerGame"
#> [19] "ppGoalsPerGame"         "shGoalsAgainstPerGame"  "ppNetGoalsPerGame"     
#> [22] "powerPlayPct"           "powerPlayNetPct"       
#> 
#> $teamReportData$powerplay$season$resultFilters
#>  [1] "gamesPlayed"            "wins"                   "losses"                
#>  [4] "ties"                   "otLosses"               "points"                
#>  [7] "pointPct"               "ppOpportunities"        "powerPlayGoalsFor"     
#> [10] "shGoalsAgainst"         "ppNetGoals"             "ppTimeOnIcePerGame"    
#> [13] "ppOpportunitiesPerGame" "ppGoalsPerGame"         "shGoalsAgainstPerGame" 
#> [16] "ppNetGoalsPerGame"      "powerPlayPct"           "powerPlayNetPct"       
#> 
#> $teamReportData$powerplay$season$sortKeys
#> [1] "powerPlayPct"
#> 
#> 
#> 
#> $teamReportData$savePercentage
#> $teamReportData$savePercentage$game
#> $teamReportData$savePercentage$game$displayItems
#>  [1] "teamId"               "franchiseId"          "teamFullName"        
#>  [4] "franchiseName"        "gameId"               "opponentTeamAbbrev"  
#>  [7] "gameDate"             "gamesPlayed"          "wins"                
#> [10] "losses"               "ties"                 "otLosses"            
#> [13] "points"               "pointPct"             "shotsAgainst"        
#> [16] "goalieGoalsAgainst"   "emptyNetGoalsAgainst" "goalsAgainst"        
#> [19] "saves"                "savePct"              "timeOnIce"           
#> [22] "goalsAgainstAverage"  "goalsAgainstPerGame"  "shutouts"            
#> 
#> $teamReportData$savePercentage$game$resultFilters
#>  [1] "gamesPlayed"          "wins"                 "losses"              
#>  [4] "ties"                 "otLosses"             "points"              
#>  [7] "pointPct"             "shotsAgainst"         "goalieGoalsAgainst"  
#> [10] "emptyNetGoalsAgainst" "goalsAgainst"         "saves"               
#> [13] "savePct"              "timeOnIce"            "goalsAgainstAverage" 
#> [16] "goalsAgainstPerGame"  "shutouts"            
#> 
#> $teamReportData$savePercentage$game$sortKeys
#> [1] "savePct"      "shotsAgainst"
#> 
#> 
#> $teamReportData$savePercentage$season
#> $teamReportData$savePercentage$season$displayItems
#>  [1] "teamId"               "franchiseId"          "teamFullName"        
#>  [4] "franchiseName"        "seasonId"             "gamesPlayed"         
#>  [7] "wins"                 "losses"               "ties"                
#> [10] "otLosses"             "points"               "pointPct"            
#> [13] "shotsAgainst"         "goalieGoalsAgainst"   "emptyNetGoalsAgainst"
#> [16] "goalsAgainst"         "saves"                "savePct"             
#> [19] "timeOnIce"            "goalsAgainstAverage"  "goalsAgainstPerGame" 
#> [22] "shutouts"            
#> 
#> $teamReportData$savePercentage$season$resultFilters
#>  [1] "gamesPlayed"          "wins"                 "losses"              
#>  [4] "ties"                 "otLosses"             "points"              
#>  [7] "pointPct"             "shotsAgainst"         "goalieGoalsAgainst"  
#> [10] "emptyNetGoalsAgainst" "goalsAgainst"         "saves"               
#> [13] "savePct"              "timeOnIce"            "goalsAgainstAverage" 
#> [16] "goalsAgainstPerGame"  "shutouts"            
#> 
#> $teamReportData$savePercentage$season$sortKeys
#> [1] "savePct"      "shotsAgainst"
#> 
#> 
#> 
#> $teamReportData$penaltykill
#> $teamReportData$penaltykill$game
#> $teamReportData$penaltykill$game$displayItems
#>  [1] "teamId"                  "franchiseId"            
#>  [3] "teamFullName"            "franchiseName"          
#>  [5] "gameId"                  "opponentTeamAbbrev"     
#>  [7] "gameDate"                "gamesPlayed"            
#>  [9] "wins"                    "losses"                 
#> [11] "ties"                    "otLosses"               
#> [13] "points"                  "pointsPct"              
#> [15] "timesShorthanded"        "ppGoalsAgainst"         
#> [17] "shGoalsFor"              "pkNetGoals"             
#> [19] "pkTimeOnIcePerGame"      "timesShorthandedPerGame"
#> [21] "ppGoalsAgainstPerGame"   "shGoalsForPerGame"      
#> [23] "pkNetGoalsPerGame"       "penaltyKillPct"         
#> [25] "penaltyKillNetPct"      
#> 
#> $teamReportData$penaltykill$game$resultFilters
#>  [1] "gamesPlayed"             "wins"                   
#>  [3] "losses"                  "ties"                   
#>  [5] "otLosses"                "points"                 
#>  [7] "pointsPct"               "timesShorthanded"       
#>  [9] "ppGoalsAgainst"          "shGoalsFor"             
#> [11] "pkNetGoals"              "pkTimeOnIcePerGame"     
#> [13] "timesShorthandedPerGame" "ppGoalsAgainstPerGame"  
#> [15] "shGoalsForPerGame"       "pkNetGoalsPerGame"      
#> [17] "penaltyKillPct"          "penaltyKillNetPct"      
#> 
#> $teamReportData$penaltykill$game$sortKeys
#> [1] "penaltyKillPct"
#> 
#> 
#> $teamReportData$penaltykill$season
#> $teamReportData$penaltykill$season$displayItems
#>  [1] "teamId"                  "franchiseId"            
#>  [3] "teamFullName"            "franchiseName"          
#>  [5] "seasonId"                "gamesPlayed"            
#>  [7] "wins"                    "losses"                 
#>  [9] "ties"                    "otLosses"               
#> [11] "points"                  "pointsPct"              
#> [13] "timesShorthanded"        "ppGoalsAgainst"         
#> [15] "shGoalsFor"              "pkNetGoals"             
#> [17] "pkTimeOnIcePerGame"      "timesShorthandedPerGame"
#> [19] "ppGoalsAgainstPerGame"   "shGoalsForPerGame"      
#> [21] "pkNetGoalsPerGame"       "penaltyKillPct"         
#> [23] "penaltyKillNetPct"      
#> 
#> $teamReportData$penaltykill$season$resultFilters
#>  [1] "gamesPlayed"             "wins"                   
#>  [3] "losses"                  "ties"                   
#>  [5] "otLosses"                "points"                 
#>  [7] "pointsPct"               "timesShorthanded"       
#>  [9] "ppGoalsAgainst"          "shGoalsFor"             
#> [11] "pkNetGoals"              "pkTimeOnIcePerGame"     
#> [13] "timesShorthandedPerGame" "ppGoalsAgainstPerGame"  
#> [15] "shGoalsForPerGame"       "pkNetGoalsPerGame"      
#> [17] "penaltyKillPct"          "penaltyKillNetPct"      
#> 
#> $teamReportData$penaltykill$season$sortKeys
#> [1] "penaltyKillPct"
#> 
#> 
#> 
#> $teamReportData$goalsagainstbystrengthgoaliepull
#> $teamReportData$goalsagainstbystrengthgoaliepull$game
#> $teamReportData$goalsagainstbystrengthgoaliepull$game$displayItems
#>  [1] "teamId"               "franchiseId"          "teamFullName"        
#>  [4] "franchiseName"        "gameId"               "opponentTeamAbbrev"  
#>  [7] "gameDate"             "gamesPlayed"          "wins"                
#> [10] "losses"               "ties"                 "otLosses"            
#> [13] "points"               "pointPct"             "goalsFor"            
#> [16] "goalsAgainstAllPulls" "goalsAgainst6On5"     "goalsAgainst6On4"    
#> [19] "goalsAgainst6On3"     "goalsAgainst3On6"     "goalsAgainst4On6"    
#> [22] "goalsAgainst5On6"     "goalsAgainst5On4"     "goalsAgainst5On3"    
#> [25] "goalsAgainst4On5"     "goalsAgainst4On3"     "goalsAgainst3On4"    
#> [28] "goalsAgainst3On5"     "goalsAgainst6On6"     "goalsAgainst5On5"    
#> [31] "goalsAgainst4On4"     "goalsAgainstPerGame" 
#> 
#> $teamReportData$goalsagainstbystrengthgoaliepull$game$resultFilters
#>  [1] "gamesPlayed"          "wins"                 "losses"              
#>  [4] "ties"                 "otLosses"             "points"              
#>  [7] "pointPct"             "goalsFor"             "goalsAgainstAllPulls"
#> [10] "goalsAgainst6On5"     "goalsAgainst6On4"     "goalsAgainst6On3"    
#> [13] "goalsAgainst3On6"     "goalsAgainst4On6"     "goalsAgainst5On6"    
#> [16] "goalsAgainst5On4"     "goalsAgainst5On3"     "goalsAgainst4On5"    
#> [19] "goalsAgainst4On3"     "goalsAgainst3On4"     "goalsAgainst3On5"    
#> [22] "goalsAgainst6On6"     "goalsAgainst5On5"     "goalsAgainst4On4"    
#> [25] "goalsAgainstPerGame" 
#> 
#> $teamReportData$goalsagainstbystrengthgoaliepull$game$sortKeys
#> list()
#> 
#> 
#> $teamReportData$goalsagainstbystrengthgoaliepull$season
#> $teamReportData$goalsagainstbystrengthgoaliepull$season$displayItems
#>  [1] "teamId"               "franchiseId"          "teamFullName"        
#>  [4] "franchiseName"        "seasonId"             "gamesPlayed"         
#>  [7] "wins"                 "losses"               "ties"                
#> [10] "otLosses"             "points"               "pointPct"            
#> [13] "goalsFor"             "goalsAgainstAllPulls" "goalsAgainst6On5"    
#> [16] "goalsAgainst6On4"     "goalsAgainst6On3"     "goalsAgainst3On6"    
#> [19] "goalsAgainst4On6"     "goalsAgainst5On6"     "goalsAgainst5On4"    
#> [22] "goalsAgainst5On3"     "goalsAgainst4On5"     "goalsAgainst4On3"    
#> [25] "goalsAgainst3On4"     "goalsAgainst3On5"     "goalsAgainst6On6"    
#> [28] "goalsAgainst5On5"     "goalsAgainst4On4"     "goalsAgainstPerGame" 
#> 
#> $teamReportData$goalsagainstbystrengthgoaliepull$season$resultFilters
#>  [1] "gamesPlayed"          "wins"                 "losses"              
#>  [4] "ties"                 "otLosses"             "points"              
#>  [7] "pointPct"             "goalsFor"             "goalsAgainstAllPulls"
#> [10] "goalsAgainst6On5"     "goalsAgainst6On4"     "goalsAgainst6On3"    
#> [13] "goalsAgainst3On6"     "goalsAgainst4On6"     "goalsAgainst5On6"    
#> [16] "goalsAgainst5On4"     "goalsAgainst5On3"     "goalsAgainst4On5"    
#> [19] "goalsAgainst4On3"     "goalsAgainst3On4"     "goalsAgainst3On5"    
#> [22] "goalsAgainst6On6"     "goalsAgainst5On5"     "goalsAgainst4On4"    
#> [25] "goalsAgainstPerGame" 
#> 
#> $teamReportData$goalsagainstbystrengthgoaliepull$season$sortKeys
#> list()
#> 
#> 
#> 
#> $teamReportData$faceoffwins
#> $teamReportData$faceoffwins$game
#> $teamReportData$faceoffwins$game$displayItems
#>  [1] "teamId"                     "franchiseId"               
#>  [3] "teamFullName"               "franchiseName"             
#>  [5] "gameId"                     "opponentTeamAbbrev"        
#>  [7] "gameDate"                   "gamesPlayed"               
#>  [9] "totalFaceoffs"              "faceoffsWon"               
#> [11] "faceoffsLost"               "faceoffWinPct"             
#> [13] "evFaceoffs"                 "evFaceoffsWon"             
#> [15] "evFaceoffsLost"             "ppFaceoffs"                
#> [17] "ppFaceoffsWon"              "ppFaceoffsLost"            
#> [19] "shFaceoffs"                 "shFaceoffsWon"             
#> [21] "shFaceoffsLost"             "offensiveZoneFaceoffs"     
#> [23] "offensiveZoneFaceoffWins"   "offensiveZoneFaceoffLosses"
#> [25] "neutralZoneFaceoffs"        "neutralZoneFaceoffWins"    
#> [27] "neutralZoneFaceoffLosses"   "defensiveZoneFaceoffs"     
#> [29] "defensiveZoneFaceoffWins"   "defensiveZoneFaceoffLosses"
#> 
#> $teamReportData$faceoffwins$game$resultFilters
#>  [1] "gamesPlayed"                "totalFaceoffs"             
#>  [3] "faceoffsWon"                "faceoffsLost"              
#>  [5] "faceoffWinPct"              "evFaceoffs"                
#>  [7] "evFaceoffsWon"              "evFaceoffsLost"            
#>  [9] "ppFaceoffs"                 "ppFaceoffsWon"             
#> [11] "ppFaceoffsLost"             "shFaceoffs"                
#> [13] "shFaceoffsWon"              "shFaceoffsLost"            
#> [15] "offensiveZoneFaceoffs"      "offensiveZoneFaceoffWins"  
#> [17] "offensiveZoneFaceoffLosses" "neutralZoneFaceoffs"       
#> [19] "neutralZoneFaceoffWins"     "neutralZoneFaceoffLosses"  
#> [21] "defensiveZoneFaceoffs"      "defensiveZoneFaceoffWins"  
#> [23] "defensiveZoneFaceoffLosses"
#> 
#> $teamReportData$faceoffwins$game$sortKeys
#> [1] "faceoffsWon"   "faceoffWinPct"
#> 
#> 
#> $teamReportData$faceoffwins$season
#> $teamReportData$faceoffwins$season$displayItems
#>  [1] "teamId"                     "franchiseId"               
#>  [3] "teamFullName"               "franchiseName"             
#>  [5] "seasonId"                   "gamesPlayed"               
#>  [7] "totalFaceoffs"              "faceoffsWon"               
#>  [9] "faceoffsLost"               "faceoffWinPct"             
#> [11] "evFaceoffs"                 "evFaceoffsWon"             
#> [13] "evFaceoffsLost"             "ppFaceoffs"                
#> [15] "ppFaceoffsWon"              "ppFaceoffsLost"            
#> [17] "shFaceoffs"                 "shFaceoffsWon"             
#> [19] "shFaceoffsLost"             "offensiveZoneFaceoffs"     
#> [21] "offensiveZoneFaceoffWins"   "offensiveZoneFaceoffLosses"
#> [23] "neutralZoneFaceoffs"        "neutralZoneFaceoffWins"    
#> [25] "neutralZoneFaceoffLosses"   "defensiveZoneFaceoffs"     
#> [27] "defensiveZoneFaceoffWins"   "defensiveZoneFaceoffLosses"
#> 
#> $teamReportData$faceoffwins$season$resultFilters
#>  [1] "gamesPlayed"                "totalFaceoffs"             
#>  [3] "faceoffsWon"                "faceoffsLost"              
#>  [5] "faceoffWinPct"              "evFaceoffs"                
#>  [7] "evFaceoffsWon"              "evFaceoffsLost"            
#>  [9] "ppFaceoffs"                 "ppFaceoffsWon"             
#> [11] "ppFaceoffsLost"             "shFaceoffs"                
#> [13] "shFaceoffsWon"              "shFaceoffsLost"            
#> [15] "offensiveZoneFaceoffs"      "offensiveZoneFaceoffWins"  
#> [17] "offensiveZoneFaceoffLosses" "neutralZoneFaceoffs"       
#> [19] "neutralZoneFaceoffWins"     "neutralZoneFaceoffLosses"  
#> [21] "defensiveZoneFaceoffs"      "defensiveZoneFaceoffWins"  
#> [23] "defensiveZoneFaceoffLosses"
#> 
#> $teamReportData$faceoffwins$season$sortKeys
#> [1] "faceoffsWon"   "faceoffWinPct"
#> 
#> 
#> 
#> $teamReportData$goalsforbystrength
#> $teamReportData$goalsforbystrength$game
#> $teamReportData$goalsforbystrength$game$displayItems
#>  [1] "teamId"                "franchiseId"           "teamFullName"         
#>  [4] "franchiseName"         "gameId"                "opponentTeamAbbrev"   
#>  [7] "gameDate"              "gamesPlayed"           "wins"                 
#> [10] "losses"                "ties"                  "otLosses"             
#> [13] "points"                "pointPct"              "goalsFor"             
#> [16] "goalsAgainst"          "goalsFor5On5"          "goalsFor4On4"         
#> [19] "goalsFor3On3"          "goalsFor5On4"          "goalsFor5On3"         
#> [22] "goalsFor4On3"          "goalsFor3On4"          "goalsFor3On5"         
#> [25] "goalsFor4On5"          "goalsForPenaltyShots"  "goalsForEmptyNet"     
#> [28] "goalsForExtraAttacker" "goalsForPerGame"      
#> 
#> $teamReportData$goalsforbystrength$game$resultFilters
#>  [1] "gamesPlayed"           "wins"                  "losses"               
#>  [4] "ties"                  "otLosses"              "points"               
#>  [7] "pointPct"              "goalsFor"              "goalsAgainst"         
#> [10] "goalsFor5On5"          "goalsFor4On4"          "goalsFor3On3"         
#> [13] "goalsFor5On4"          "goalsFor5On3"          "goalsFor4On3"         
#> [16] "goalsFor3On4"          "goalsFor3On5"          "goalsFor4On5"         
#> [19] "goalsForPenaltyShots"  "goalsForEmptyNet"      "goalsForExtraAttacker"
#> [22] "goalsForPerGame"      
#> 
#> $teamReportData$goalsforbystrength$game$sortKeys
#> [1] "goalsFor"
#> 
#> 
#> $teamReportData$goalsforbystrength$season
#> $teamReportData$goalsforbystrength$season$displayItems
#>  [1] "teamId"                "franchiseId"           "teamFullName"         
#>  [4] "franchiseName"         "seasonId"              "gamesPlayed"          
#>  [7] "wins"                  "losses"                "ties"                 
#> [10] "otLosses"              "points"                "pointPct"             
#> [13] "goalsFor"              "goalsAgainst"          "goalsFor5On5"         
#> [16] "goalsFor4On4"          "goalsFor3On3"          "goalsFor5On4"         
#> [19] "goalsFor5On3"          "goalsFor4On3"          "goalsFor3On4"         
#> [22] "goalsFor3On5"          "goalsFor4On5"          "goalsForPenaltyShots" 
#> [25] "goalsForEmptyNet"      "goalsForExtraAttacker" "goalsForPerGame"      
#> 
#> $teamReportData$goalsforbystrength$season$resultFilters
#>  [1] "gamesPlayed"           "wins"                  "losses"               
#>  [4] "ties"                  "otLosses"              "points"               
#>  [7] "pointPct"              "goalsFor"              "goalsAgainst"         
#> [10] "goalsFor5On5"          "goalsFor4On4"          "goalsFor3On3"         
#> [13] "goalsFor5On4"          "goalsFor5On3"          "goalsFor4On3"         
#> [16] "goalsFor3On4"          "goalsFor3On5"          "goalsFor4On5"         
#> [19] "goalsForPenaltyShots"  "goalsForEmptyNet"      "goalsForExtraAttacker"
#> [22] "goalsForPerGame"      
#> 
#> $teamReportData$goalsforbystrength$season$sortKeys
#> [1] "goalsFor"
#> 
#> 
#> 
#> 
#> $aggregatedColumns
#>  [1] "gameId"             "teamAbbrev"         "currentTeamAbbrev" 
#>  [4] "seasonId"           "gameDate"           "opponentTeamAbbrev"
#>  [7] "teamAbbrevs"        "teamId"             "teamFullName"      
#> [10] "homeRoad"           "currentTeamName"   
#> 
#> $individualColumns
#> [1] "franchiseId"   "franchiseName"
#> 
# }
```
