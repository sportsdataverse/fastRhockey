## the functions here still need work to account for the variation
## in how the NWHL/PHF does weird data structure stuff

# shot_data <- function(data) {
#
#   score <- data[[max(length(data)) - 2]]
#   shot <- data[[max(length(data)) - 1]]
#
#   score <- score %>%
#     clean_names() %>%
#     rename("team" = "scoring",
#            "first_scoring" = "x1st",
#            "second_scoring" = "x2nd",
#            "third_scoring" = "x3rd",
#            "total_scoring" = "t")
#
#   shot <- shot %>%
#     clean_names() %>%
#     rename("team" = "shots",
#            "first_shots" = "x1st",
#            "second_shots" = "x2nd",
#            "third_shots" = "x3rd",
#            "total_shots" = "t")
#
#   shot %>%
#     left_join(score, by = c("team"))
#
# }


