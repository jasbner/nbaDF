update_player_agg <- function(date = as.character(Sys.Date())) {
  data("games_details")

  players <- games_details |>
    select(PLAYER_ID, PLAYER_NAME) |>
    unique() |>
    mutate(PLAYER_NAME_simplified = stringr::str_extract(PLAYER_NAME, "[:graph:]+ [:graph:]+"))
  # take the most recent player name as thats most likely the one to be in the nba currently
  players <- players |>
    group_by(PLAYER_NAME_simplified) |>
    filter(PLAYER_ID == max(PLAYER_ID)) |>
    ungroup()

  #RJ hampton needs to remove periods from name 1630181
  players <- players |> mutate(PLAYER_NAME_simplified = ifelse(PLAYER_ID == 1630181,"RJ Hampton",PLAYER_NAME_simplified))

  FD_players <- load_todays_fanduel(date = date)
  FD_players <- FD_players |>
    select(Id, Nickname) |>
    unique()


  combined_players <- FD_players |> left_join(players, by = c("Nickname" = "PLAYER_NAME_simplified"))
  # these players were not found
  combined_players |> filter(is.na(PLAYER_ID))

  # ground truth for now is FD
  player_agg <- combined_players |> select(FD_Id = Id, FD_Nickname = Nickname, NBA_PLAYER_ID = PLAYER_ID, NBA_PLAYER_NAME = PLAYER_NAME)


  usethis::use_data(player_agg, overwrite = TRUE)

  player_agg
}
