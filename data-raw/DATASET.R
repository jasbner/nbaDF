# read in player data
games_details <- readr::read_csv("./data-raw/games_details.csv")
# save data to rdata file
usethis::use_data(games_details)

# read in game data
games <- readr::read_csv("./data-raw/games.csv")
# save data to rdata file
usethis::use_data(games)
# now we need to fill the gaps in games
# get array of dates since the last game in the our stored dataset
library(tidyverse)
devtools::load_all()
data(games)
if (max(games$GAME_DATE_EST) + 1 <= (date(Sys.time()) - 1)) {
  new_games <- seq.Date(max(games$GAME_DATE_EST) + 1, date(Sys.time()) - 1, by = "day")
  urls <- paste0("https://stats.nba.com/stats/scoreboardV2?DayOffset=0&LeagueID=00&gameDate=", new_games)

  headers <- c(
    `Host` = "stats.nba.com",
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0",
    `Accept` = "application/json, text/plain, */*",
    `Accept-Language` = "en-US,en;q=0.5",
    `Accept-Encoding` = "gzip, deflate, br",
    `x-nba-stats-origin` = "stats",
    `x-nba-stats-token` = "true",
    `Connection` = "keep-alive",
    `Referer` = "https =//stats.nba.com/",
    `Pragma` = "no-cache",
    `Cache-Control` = "no-cache"
  )

  library(httr2)
  for (url in urls) {
    # create basis for request
    req <- httr2::request(url)
    # add headers to the request
    resp <- req |>
      req_headers(!!!headers) |>
      req_perform() # send request
    # content of response body into json
    res <- resp |> resp_body_json()

    # if we got games data back parse it into the games dataframe
    if (length(pluck(res$resultSets, 1, "rowSet")) > 0) {
      df_names <- map_chr(res$resultSets, 1)
      df_names
      for (i in seq_along(df_names)) {
        # get names of the each category
        category_names <- pluck(res, "resultSets", i, "headers") |> unlist()

        # create tibble
        dat <- tibble(dat = pluck(res, "resultSets", i, "rowSet"))
        out <- dat %>%
          mutate(dat) |>
          unnest_wider(dat, names_sep = "val") |>
          rename_all(~category_names)

        assign(df_names[i], out)
      }

      line_id <- GameHeader |>
        select(HOME_TEAM_ID, VISITOR_TEAM_ID) |>
        pivot_longer(cols = everything(), values_to = "TEAM_ID", names_pattern = "(H|V)") |>
        mutate(name = str_replace(str_replace(name, "H", "home"), "V", "away"))
      LineScore <- LineScore |> left_join(line_id, by = "TEAM_ID")

      LineScore <- LineScore |> pivot_wider(names_from = name, values_from = c(-name, -GAME_ID, -GAME_DATE_EST))
      LineScore <- LineScore |> unnest(everything())

      combined <- GameHeader |>
        left_join(LineScore, by = c("GAME_ID", "GAME_DATE_EST")) |>
        select_at(vars(matches(colnames(games)))) |>
        mutate(HOME_TEAM_WINS = as.double(PTS_home > PTS_away)) |>
        mutate_at(vars(c(-"GAME_STATUS_TEXT", -"GAME_DATE_EST")), as.double)

      games <- rbind(games, combined)
    }
    Sys.sleep(.6)
    print(url)
  }

  usethis::use_data(games, overwrite = TRUE)
}



# games_details -----------------------------------------------------------

#get all games that were recently updated
new_games <- games |> filter(!GAME_ID %in% games_details$GAME_ID) |> mutate(ssn_str = paste0(SEASON,"-",gsub("20","",SEASON+1)))
#filter out old games
new_games <- new_games |> filter(SEASON>2005)
new_games_str <- new_games |> pull(GAME_ID)


headers <- c(
  `Host` = "stats.nba.com",
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0",
  `Accept` = "application/json, text/plain, */*",
  `Accept-Language` = "en-US,en;q=0.5",
  `Accept-Encoding` = "gzip, deflate, br",
  `x-nba-stats-origin` = "stats",
  `x-nba-stats-token` = "true",
  `Connection` = "keep-alive",
  `Referer` = "https =//stats.nba.com/",
  `Pragma` = "no-cache",
  `Cache-Control` = "no-cache"
)

if(length(new_games)>0){
# add 00 to front of GAME_ID to fit format of stats
urls = paste0('https://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=0&GameID=00',new_games_str,'&RangeType=0&StartPeriod=1&StartRange=0')


library(httr2)
for (url in urls) {
# create basis for request
req <- httr2::request(url)
# add headers to the request
resp <- req |>
  req_headers(!!!headers) |>
  req_perform() # send request
# content of response body into json
res <- resp |> resp_body_json()

# if we got games data back parse it into the games dataframe
if (length(pluck(res$resultSets, 1, "rowSet")) > 0) {
  df_names <- map_chr(res$resultSets, 1)
  df_names
  for (i in seq_along(df_names)) {
    # get names of the each category
    category_names <- pluck(res, "resultSets", i, "headers") |> unlist()

    # create tibble
    dat <- tibble(dat = pluck(res, "resultSets", i, "rowSet"))
    out <- dat %>%
      mutate(dat) |>
      unnest_wider(dat, names_sep = "val") |>
      rename_all(~category_names)

    assign(df_names[i], out)
  }

  games_details <- rbind(games_details, PlayerStats)
}
Sys.sleep(.6)
print(url)
}
#need to set game id back to numeric because it's a charcter when it comes back from api
# potential issue with this is that we loose the leading 00 for the game id
games_details <- games_details |> mutate(GAME_ID = as.numeric(GAME_ID))
usethis::use_data(games_details, overwrite = TRUE)

}


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
