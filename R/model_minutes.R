model_minutes <- function(game_stats){
  # get the last game played for everyone this year
  fp_min <- game_stats |> arrange(desc(GAME_DATE_EST)) |> group_by(PLAYER_ID) |> filter(row_number() == 1) |> ungroup() |> select(PLAYER_ID,MIN)

  fp_min
}
