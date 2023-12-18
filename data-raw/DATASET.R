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


# injury report information -----------------------------------------------

#attempt to read injury report
library(pdftools)

parse_injury_report = function(report_text){
  df <- tibble(txt = read_lines(report_text))
  df <- df |> filter(!txt == "") |> filter(!str_detect(txt,"Injury Report"))
  df_colnames <- df[1,] |> str_split("  +") |> unlist()
  dat <- df |> mutate(txt = gsub("  +","|",txt)) |> tidyr::separate_wider_delim(txt,"|", too_few = "align_end", names = df_colnames)
  dat <- dat |> filter(!is.na(`Player Name`)) |> filter(!`Player Name` == "") |> select(-`Game Time`,-`Matchup`)
  dat <- dat |> filter(grepl(",",`Player Name`))
  dat <- dat |>  mutate_all(na_if,"")
  dat <- dat |> fill(`Game Date`) |> fill(Team)
  if("Category" %in% colnames(dat)){
    dat <- dat |> unite("Reason",c(Category,Reason), sep = " - ")
  }
  if(!"Previous Status" %in% colnames(dat)){
    dat <- dat |> mutate(`Previous Status` = NA_character_)
  }
  if("Previous Reason" %in% colnames(dat)){
  dat <- dat |> select(-`Previous Reason`)
  }
  dat
}

files <- list.files("data-raw/injury_reports",pattern = "pdf$", full.names = TRUE)
res_df <- parse_injury_report(pdf_text(files[1]))
res_df$report_date <- files[1] |> str_extract("\\d+-\\d+-\\d+") |> as_date()
files <- files[-1]
for(f in files){
  print(f)
  res <- parse_injury_report(pdf_text(f))
  res$report_date <- f |> str_extract("\\d+-\\d+-\\d+") |> as_date()
  res_df <- rbind(res_df,res)

}

hist_injury_report <- res_df
usethis::use_data(hist_injury_report, overwrite = TRUE)
