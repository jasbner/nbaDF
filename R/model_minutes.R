#' Title
#'
#' @param game_stats
#'
#' @import dplyr
#' @import zoo
#' @import randomForest
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
model_minutes <- function(game_stats) {
  # get the last game played for everyone this year
  # fp_min <- game_stats |> arrange(desc(GAME_DATE_EST)) |> group_by(PLAYER_ID) |> filter(row_number() == 1) |> ungroup() |> select(PLAYER_ID,MIN)

  fp_min <- game_stats |> select(PLAYER_ID, TEAM_ID, GAME_DATE_EST, MIN)

  # get the set of games that were played for each team
  game_dates <- fp_min |>
    select(TEAM_ID, GAME_DATE_EST) |>
    unique()

  # This takes a long time, wonder if we can speed this up or precompute it?
  # it basically fills all players dates for every day and game that was played but
  # we could minimize this based on the players career dates or season date if we pulled in another column
  filled <- fp_min |>
    group_by(TEAM_ID, PLAYER_ID) |>
    tidyr::complete(GAME_DATE_EST = seq.Date(min(GAME_DATE_EST), max(GAME_DATE_EST), by = "days"))

  # get the players minutes if they did not play
  zeros_added <- filled |> inner_join(game_dates, by = c("TEAM_ID" = "TEAM_ID", "GAME_DATE_EST" = "GAME_DATE_EST"))

  # remove players that only had 1 game with a team
  zeros_added <- zeros_added |>
    ungroup() |>
    group_by(TEAM_ID, PLAYER_ID) |>
    filter(!sum(!is.na(MIN)) == 1) |>
    ungroup()

  # remove players that had less than 5 games with a team
  zeros_added <- zeros_added |>
    group_by(PLAYER_ID, TEAM_ID) |>
    filter(n() > 5)

  # interpolate na values
  zeros_added$MIN_na_removed <- zoo::na.approx(zeros_added$MIN)

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

  # create model
  # reduce observations because it takes too long to train
  train_data <- features |>
    filter(year(GAME_DATE_EST) > 2020) |>
    select(-TEAM_ID, -PLAYER_ID, -GAME_DATE_EST)

  # separate out predictors and target for random forest
  train_predictors <- train_data |> select(-value_pred)
  value_pred <- train_data$value_pred

  if (refit) {
    rf.fit <- randomForest(train_predictors, value_pred, ntree = 100, keep.forest = TRUE, importance = TRUE)
  }

  # use model to predict todays minutes played
  predict_data <- fp_min |>
    ungroup() |>
    select(-TEAM_ID, -PLAYER_ID, -GAME_DATE_EST, -MIN, -value_pred)
  fp_pred <- predict(rf.fit, predict_data)


  final_predictions <- tibble(PLAYER_ID = fp_min$PLAYER_ID, MIN = fp_pred)
  final_predictions

}
