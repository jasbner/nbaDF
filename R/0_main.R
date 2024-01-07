#' Main function
#'
#' @return
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
update_run_full_nba_model <- function(){
  tictoc::tic()
  #set date for when we want to predict (sometimes we'll predict a day or two out)
  date_predict <- Sys.Date()

  #load training data
  game_stats <- load_training_data()

  #build fantasy points per minute model
  fp_pred <- model_fp_per_min(game_stats)

  #build minutes model
  min_pred <- model_minutes(game_stats, refit = FALSE)
  # min_pred <- model_minutes(game_stats, refit = TRUE)

  #load today's fanduel numbers
  dat <- load_todays_fanduel(date_predict)

  #load player id aggregation
  player_agg <- update_player_agg(date_predict)

  #join to prediction
  dat_pred <- dat |> left_join(player_agg, by = c("Id" = "FD_Id")) |> left_join(fp_pred, by = c("NBA_PLAYER_ID" = "PLAYER_ID")) |> left_join(min_pred, by = c("NBA_PLAYER_ID" = "PLAYER_ID"))

  # filter out players with insufficient data or low likelihood to end up on final roster
  # need to update this once predictions are in place
  dat_pred_filtered <- filter_roster_before_optim(dat_pred)

  #submit to optimization routine
  res <- optim_nba(dat = dat_pred_filtered, n_fantasy_teams = 5, p = 7)

  #format data so it fits fanduel csv format
  fd_ss <- create_fanduel_lineup_spreadsheet(res)

  #write to file
  #update spreadsheet if necessary
  update_csv(fd_ss,date_predict)

  write.csv(fd_ss, file = paste0("./data-raw/generated_lineups/lineup-",date_predict,".csv"), row.names = FALSE)

  tictoc::toc()
  #summarise results
  res |> group_by(Nickname) |> count() |> arrange(desc(n))
}
