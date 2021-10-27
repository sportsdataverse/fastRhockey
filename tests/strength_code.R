# messy_data <- read.csv("~/whockey_scraper/data-raw/data templates/messy_data.csv")
#
# away_state_changes <- messy_data %>%
#   filter((event == "PP Goal" & str_sub(team,-3) == home_team) |
#            (event == "Penalty" & str_sub(team,-3) == away_team))%>%
#   select(event,sec_from_start,power_play_seconds) %>%
#   mutate(event = ifelse(event == "Penalty",1,2),
#          prev.event = lag(event),
#          prev.time = lag(sec_from_start),
#          prev.length = lag(power_play_seconds))
#
#
# away_pen_mat <- apply(away_state_changes,
#                       1,
#                       FUN = function(x) {
#                         #Creates a -1 for duration of penalty and 0s surrounding it
#                         if(x[1] == 1 & x[2]+x[3]*60 < (max(messy_data$period_id)*1200-1)){
#                           c( rep( 0, length( 0:x[2] )),
#                              rep( -1, x[3]*60),
#                              rep(0, length((x[2]+x[3]*60 + 1):(max(messy_data$period_id)*1200-1)))
#                           )
#                           #Creates a -1 for duration of penalty and 0s before (for end of game penalties)
#                         } else if(x[1] == 1 & x[2]+x[3]*60 >= (max(messy_data$period_id)*1200-1)) {
#                           c( rep( 0, length( 0:x[2] )),
#                              rep(-1, max(messy_data$period_id)*1200-1-x[2] )
#                           )
#                           #Creates a +1 from time power play goal is scored to end of penalty to handle skater coming back on
#                         } else if( x[1] == 2 & (x[2] %in% ifelse(!is.na(x[5]) & !is.na(x[6]) & x[2] != x[5], x[5]:(x[5]+x[6]*60),-1 )) ) {
#                           c( rep( 0, length( 0:(x[2]) )),
#                              rep( 1, length( (x[2]+1):(x[6]*60-(x[2]-x[5])))),
#                              rep(0, length((x[6]*60-(x[2]-x[5])):(max(messy_data$period_id)*1200-1)))
#                           )
#                           # Creates all zeros if event doesnt effect strength
#                         } else {
#                           rep(0, length(0:(max(messy_data$period_id)*1200-1)))
#                         }
#                       })
#
# #creates vector for skaters
# away_skaters <- 5 + apply(away_pen_mat, 1, sum)
#
#
# away_skaters <- as.data.frame(away_skaters) %>%
#   rownames_to_column("sec_from_start")%>%
#   mutate(sec_from_start = as.numeric(sec_from_start))
#
# home_state_changes <- messy_data %>%
#   filter((event == "PP Goal" & str_sub(team,-3) == away_team) |
#            (event == "Penalty" & str_sub(team,-3) == home_team))%>%
#   select(event,sec_from_start,power_play_seconds) %>%
#   mutate(event = ifelse(event == "Penalty",1,2),
#          prev.event = lag(event),
#          prev.time = lag(sec_from_start),
#          prev.length = lag(power_play_seconds))
#
#
# home_pen_mat <- apply(home_state_changes,
#                       1,
#                       FUN = function(x) {
#                         #Creates a -1 for duration of penalty and 0s surrounding it
#                         if(x[1] == 1 & x[2]+x[3]*60 < (max(messy_data$period_id)*1200-1)){
#                           c( rep( 0, length( 0:x[2] )),
#                              rep( -1, x[3]*60),
#                              rep(0, length((x[2]+x[3]*60 + 1):(max(messy_data$period_id)*1200-1)))
#                           )
#                           #Creates a -1 for duration of penalty and 0s before (for end of game penalties)
#                         } else if(x[1] == 1 & x[2]+x[3]*60 >= (max(messy_data$period_id)*1200-1)) {
#                           c( rep( 0, length( 0:x[2] )),
#                              rep(-1, max(messy_data$period_id)*1200-1-x[2] )
#                           )
#                           #Creates a +1 from time power play goal is scored to end of penalty to handle skater coming back on
#                         } else if( x[1] == 2 & (x[2] %in% ifelse(!is.na(x[5]) & !is.na(x[6]) & x[2] != x[5], x[5]:(x[5]+x[6]*60),-1 )) ) {
#                           c( rep( 0, length( 0:(x[2]) )),
#                              rep( 1, length( (x[2]+1):(x[6]*60-(x[2]-x[5])))),
#                              rep(0, length((x[6]*60-(x[2]-x[5])):(max(messy_data$period_id)*1200-1)))
#                           )
#                           # Creates all zeros if event doesnt effect strength
#                         } else {
#                           rep(0, length(0:(max(messy_data$period_id)*1200-1)))
#                         }
#                       })
#
# #creates vector for skaters
# home_skaters <- 5 + apply(home_pen_mat, 1, sum)
#
# home_skaters <- as.data.frame(home_skaters) %>%
#   rownames_to_column("sec_from_start")%>%
#   mutate(sec_from_start = as.numeric(sec_from_start))
#
#
#
# messy_data<- left_join(messy_data, home_skaters)
# messy_data<- left_join(messy_data, away_skaters)
