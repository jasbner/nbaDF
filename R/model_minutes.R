#' Title
#'
#' @param game_stats
#'
#' @import dplyr
#' @import zoo
#' @import randomForest
#' @import tidyr
#'
#' @return
#' @export
#'
#' @examples
model_minutes <- function(game_stats, refit = FALSE) {
  # get the last game played for everyone this year
  # fp_min <- game_stats |> arrange(desc(GAME_DATE_EST)) |> group_by(PLAYER_ID) |> filter(row_number() == 1) |> ungroup() |> select(PLAYER_ID,MIN)

  fp_min <- game_stats |> select(PLAYER_ID, TEAM_ID, GAME_DATE_EST, MIN, START_POSITION,SEASON)


  # get the set of games that were played for each team
  game_dates <- fp_min |>
    select(TEAM_ID, GAME_DATE_EST) |>
    unique()

  # If a player played this season, extrapolate the dates to the current date
  last_game <- game_stats |> group_by(TEAM_ID) |> arrange(desc(GAME_DATE_EST)) |> filter(row_number() == 1) |> select(TEAM_ID,GAME_DATE_EST,SEASON) |> rename(last_game = GAME_DATE_EST, last_game_season = SEASON)
  curr_season <- max(game_stats$SEASON, na.rm = TRUE)

  #build last day played to current
  add_upper_bound <- game_stats |> left_join(last_game, by = c("TEAM_ID" = "TEAM_ID")) |> group_by(TEAM_ID, PLAYER_ID) |> mutate(new_game_date_est = as_date(ifelse((SEASON == last_game_season)& (GAME_DATE_EST == max(GAME_DATE_EST)),last_game,GAME_DATE_EST)))

  # This takes a long time, wonder if we can speed this up or precompute it?
  # it basically fills all players dates for every day and game that was played but
  # we could minimize this based on the players career dates or season date if we pulled in another column
  filled <- add_upper_bound |>
    group_by(TEAM_ID, PLAYER_ID) |> select(PLAYER_ID, TEAM_ID, GAME_DATE_EST, MIN, START_POSITION,SEASON,new_game_date_est) |>
    tidyr::complete(GAME_DATE_EST = seq.Date(min(GAME_DATE_EST), max(new_game_date_est), by = "days"))


  # get the players minutes if they did not play
  zeros_added <- filled |> inner_join(game_dates, by = c("TEAM_ID" = "TEAM_ID", "GAME_DATE_EST" = "GAME_DATE_EST"))

  # # remove players that only had 1 game with a team
  # zeros_added <- zeros_added |>
  #   ungroup() |>
  #   group_by(TEAM_ID, PLAYER_ID) |>
  #   filter(!sum(!is.na(MIN)) == 1) |>
  #   ungroup()

  # remove players that had less than 5 games with a team
  zeros_added <- zeros_added |>
    group_by(PLAYER_ID, TEAM_ID) |>
    filter(n() > 5)

  # interpolate na values
  zeros_added$MIN_na_removed <- zoo::na.approx(zeros_added$MIN, na.rm = FALSE)

  #filter out those that still could not be interpolated
  zeros_added <- zeros_added |> filter(!is.na(MIN_na_removed))

  # this can be improved i couldnt figure out how to do this dynamically
  features <- zeros_added |>
    group_by(TEAM_ID, PLAYER_ID) |>
    arrange(desc(GAME_DATE_EST)) |>
    mutate(mean_lag_5 = rollmean(MIN_na_removed, k = 5, fill = "extend", align = "left")) |>
    mutate(mean_lag_3 = rollmean(MIN_na_removed, k = 3, fill = "extend", align = "left")) |>
    mutate(value = replace_na(MIN, 0)) |>
    mutate(true_mean_lag_3 = rollmean(value, k = 3, fill = "extend", align = "left")) |>
    mutate(true_mean_lag_5 = rollmean(value, k = 5, fill = "extend", align = "left")) |>
    mutate(lag_1 = lead(value)) |>
    mutate(out_last2 = as.numeric(lag_1 + lead(value, 2) == 0)) |>
    mutate(out_last2 = replace_na(out_last2, 0)) |>
    mutate(out_last3 = as.numeric(lag_1 + lead(value, 2) + lead(value, 3) == 0)) |>
    mutate(out_last3 = replace_na(out_last3, 0)) |>
    mutate(value_pred = lag(value)) |>
    filter(!is.na(lag_1)) # lets filter out the first game with a team

  # ungroup
  features <- features |> ungroup()

  # one hot encode starters
  features <- features |> mutate(starter = ifelse(START_POSITION=="",0,1)) |> mutate(starter = ifelse(is.na(START_POSITION),0,starter))


# injury report data ------------------------------------------------------
  data("hist_injury_report")

  #add the most recent available injury report for today
  today_injury_report <- update_injury_report()

  #filter out those games that are not for the report date aka games for the next
  # becaues we only want to predict for the current day
  injury_report <- hist_injury_report |> mutate(game_date = mdy(`Game Date`)) |> select(-`Game Date`)
  #reduce todays injury report by one so it joins to the last date for all of the other data
  today_injury_report <- today_injury_report |> mutate(game_date = mdy(`Game Date`) - 1, report_date = report_date - 1) |> select(-`Game Date`)

  today_injury_report <- today_injury_report |> select(colnames(injury_report))

  #join together
  injury_report <- rbind(injury_report,today_injury_report)

  #merge team id and player id
  data("team_details")

  # set teamname similar to how it is done in the nba stats
  team_merge <- team_details |> select(TEAM_ID,CITY,NICKNAME) |> tidyr::unite("FULLNAME",c("CITY","NICKNAME"), sep = " ")

  # merge team with team details
  injury_report_id <- injury_report |> left_join(team_merge, by = c("Team" = "FULLNAME"))
  # fill gaps
  injury_report_id <- injury_report_id |> mutate(TEAM_ID = ifelse(Team == "Minnesota",1610612750,TEAM_ID)) |> mutate(TEAM_ID = ifelse(Team == "LA Clippers",1610612746,TEAM_ID))

  #get a set of player names and their associated teams
  player_names <- game_stats |> select(PLAYER_NAME,PLAYER_ID,TEAM_ID) |> unique()

  #format player name similar to game stats
  injury_report_id <- injury_report_id |> tidyr::separate(`Player Name`,c("Last","First"), extra = "merge", sep = ", ", fill = "right") |> tidyr::unite(PLAYER_NAME, c("First","Last"), sep = " ")

  # this join could probably be better
  injury_report_id <- injury_report_id |> left_join(player_names, by = c("PLAYER_NAME" = "PLAYER_NAME", "TEAM_ID" = "TEAM_ID"))

  # filter if we didnt find a match
  injury_report_id <- injury_report_id |> filter(!is.na(PLAYER_ID)) |> filter(!is.na(TEAM_ID))

  #format to join with the rest of the data
  injury_join <- injury_report_id |> select(TEAM_ID,PLAYER_ID,game_date,`Current Status`, `Previous Status`, Reason)

  #rename columns with spaces
  injury_join <- injury_join |> rename(status = `Current Status`, prev_status = `Previous Status`)

  # join back to features may need to investigate duplication
  features <- features |> left_join(injury_join, by = c("TEAM_ID" = "TEAM_ID", "PLAYER_ID" = "PLAYER_ID", "GAME_DATE_EST" = "game_date"))

  #one hot encode status this could be functionalized
  features <- features |> mutate(injury_out = ifelse(status == "Out", 1,0)) |> mutate(injury_out = replace_na(injury_out,0))
  features <- features |> mutate(injury_Available = ifelse(status == "Available", 1,0)) |> mutate(injury_Available = replace_na(injury_Available,0))
  features <- features |> mutate(injury_Doubtful = ifelse(status == "Doubtful", 1,0)) |> mutate(injury_Doubtful = replace_na(injury_Doubtful,0))
  features <- features |> mutate(injury_Probable = ifelse(status == "Probable", 1,0)) |> mutate(injury_Probable = replace_na(injury_Probable,0))
  features <- features |> mutate(injury_Questionable = ifelse(status == "Questionable", 1,0)) |> mutate(injury_Questionable = replace_na(injury_Questionable,0))

  #deselect injury columns as they are no longer needed
  features <- features |> select(-status,-prev_status,-Reason)

  # Modeling ----------------------------------------------------------------

  # get the set of values we want to predict
  fp_min <- features |>
    group_by(PLAYER_ID) |>
    arrange(desc(GAME_DATE_EST)) |>
    filter(row_number() == 1) |>
    filter(year(GAME_DATE_EST) == year(today()))

  # filter out columns with nas
  features <- features |> select(-MIN)

  # filter out unknowns
  features <- features |> filter(!is.na(value_pred))

  #filter out dates that we don't have the injury report
  features <- features |> filter(GAME_DATE_EST %in% unique(injury_join$game_date))

  # create model
  # reduce observations because it takes too long to train
  train_data <- features |>
    #filter(year(GAME_DATE_EST) > 2020) |>
    select(-TEAM_ID, -PLAYER_ID, -GAME_DATE_EST, -START_POSITION, -SEASON, -new_game_date_est)

  # separate out predictors and target for random forest
  train_predictors <- train_data |> select(-value_pred)
  value_pred <- train_data$value_pred

  #if we have new data need to refit
  if (refit) {
    rf.fit <- randomForest(train_predictors, value_pred, ntree = 500, keep.forest = TRUE, importance = TRUE)
    usethis::use_data(rf.fit,overwrite = TRUE)
  }

  data("rf.fit")
  model <- rf.fit

  # use model to predict todays minutes played
  # fp_min <- fp_min |> filter(!is.na(mean_lag_5))
  predict_data <- fp_min |>
    ungroup() |>
    select(-TEAM_ID, -PLAYER_ID, -GAME_DATE_EST, -MIN, -value_pred, -START_POSITION, -SEASON, -new_game_date_est)
  fp_pred <- predict(model, predict_data)


  final_predictions <- tibble(PLAYER_ID = fp_min$PLAYER_ID, min_pred = fp_pred)
  final_predictions

}
