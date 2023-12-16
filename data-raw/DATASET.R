# read in player data
games_details <- readr::read_csv("./data-raw/games_details.csv")
# save data to rdata file
usethis::use_data(games_details)

# read in game data
games <- readr::read_csv("./data-raw/games.csv")
# save data to rdata file
usethis::use_data(games)



# player information ------------------------------------------------------


data("games_details")

players <- games_details |> select(PLAYER_ID,PLAYER_NAME) |> unique()|> mutate(PLAYER_NAME_simplified = stringr::str_extract(PLAYER_NAME,"[:graph:]+ [:graph:]+"))
# take the most recent player name as thats most likely the one to be in the nba currently
players <- players |> group_by(PLAYER_NAME_simplified) |> filter(PLAYER_ID == max(PLAYER_ID)) |> ungroup()

FD_players <- load_todays_fanduel()
FD_players <- FD_players |> select(Id,Nickname) |> unique()


combined_players <- FD_players |> left_join(players,by = c("Nickname" = "PLAYER_NAME_simplified"))
#these players were not found
combined_players |> filter(is.na(PLAYER_ID))

# ground truth for now is FD
player_agg <- combined_players |> select(FD_Id = Id, FD_Nickname = Nickname, NBA_PLAYER_ID = PLAYER_ID, NBA_PLAYER_NAME = PLAYER_NAME)
player_agg

usethis::use_data(player_agg, overwrite = TRUE)
