#' Title
#'
#' @param game_stats
#'
#' @import dplyr
#' @import zoo
#' @return
#' @export
#'
#' @examples
model_fp_per_min <- function(game_stats){
  # get the last game played for everyone this year
  # fp_pred <- game_stats |> arrange(desc(GAME_DATE_EST)) |> group_by(PLAYER_ID) |> filter(row_number() == 1) |> ungroup() |> select(PLAYER_ID,fp_min)
  # this can be improved i couldnt figure out how to do this dynamically
  mean_lag <- game_stats |>
    group_by(TEAM_ID, PLAYER_ID) |>
    arrange(desc(GAME_DATE_EST)) |>
    filter(n()>5) |>
    mutate(fp_single_game = fp/MIN, fp_pred = rollmedian(fp_single_game, k = 5, fill = "extend", align = "left")) |>
    ungroup()

  # get the set of values we want to predict
  fp_pred <- mean_lag |>
    group_by(PLAYER_ID) |>
    arrange(desc(GAME_DATE_EST)) |>
    filter(row_number() == 1) |>
    filter(year(GAME_DATE_EST) == year(today())) |>
    ungroup()

  fp_pred <- fp_pred |> select(PLAYER_ID,fp_pred)
  fp_pred
}
