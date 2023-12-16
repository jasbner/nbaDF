#' Title
#'
#' @param dat_pred
#'
#' @import dplyr
#' @return
#' @export
#'
#' @examples
filter_roster_before_optim <- function(dat_pred){
  #filter out players that do not have stats from nba
  if(nrow(dat_pred |> filter(is.na(NBA_PLAYER_ID))) > 0){
    warning("Some players do not have stats: ", paste0(dat_pred |> filter(is.na(NBA_PLAYER_ID)) |> pull(Nickname), collapse = ","))
    dat_pred <- dat_pred |> filter(!is.na(NBA_PLAYER_ID))
  }

  #substitue prediction to FPPG as the FPPG column is what we're optimizing on
  dat_pred <- dat_pred |> mutate(FPPG = fp_min * MIN)

  #filter out players that do not have a prediction
  if(nrow(dat_pred |> filter(is.na(FPPG))) > 0){
    warning("Some players do not have predictions: ", paste0(dat_pred |> filter(is.na(FPPG)) |> pull(Nickname), collapse = ","))
    dat_pred <- dat_pred |> filter(!is.na(FPPG))
  }


  #filter out players that are unlikely to end up in the final roster
  dat_pred
}
