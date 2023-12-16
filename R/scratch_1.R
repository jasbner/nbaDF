t <- function(){
library(lubridate)
library(dplyr)
library(tidyr)
data("games_details")
data("games")
games_unique <- games |> group_by(GAME_ID) |> filter(row_number() == 1) |> ungroup()
game_stats <- games_details |> left_join(games_unique, by = "GAME_ID")
# arrange by date
game_stats <- game_stats |> arrange(desc(GAME_DATE_EST))

#replace minute data error
game_stats$MIN <- gsub(".000000","",game_stats$MIN)
game_stats <- game_stats |> mutate(MIN = replace_na(MIN,"0:0"))


game_stats <- game_stats |> mutate(MIN = as.numeric(ms(MIN))/60)
game_stats <- game_stats |> filter(!is.na(MIN))


# calculate fantasy points(fp) and fantasy points per minute (fp_min)
game_stats <- game_stats |> filter(MIN>12) |> mutate(fp = AST*1.5+BLK*3+OREB*1.2+DREB*1.2+STL*3-TO+PTS, fp_min = fp/MIN)

# get the last game played for everyone this year
fp_min_pred <- game_stats |> arrange(desc(GAME_DATE_EST)) |> group_by(PLAYER_ID) |> filter(row_number() == 1) |> ungroup() |> select(PLAYER_ID,PLAYER_NAME,fp_min, MIN) |> arrange(PLAYER_NAME)

fp_min_pred

#load today's fanduel numbers
dat <- load_todays_fanduel()

#add id column
data("player_agg")

#join to prediction
dat_pred <- dat |> left_join(player_agg, by = c("Id" = "FD_Id")) |> left_join(fp_min_pred, by = c("NBA_PLAYER_ID" = "PLAYER_ID"))

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




#submit to optimization routine
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
res <- optim_nba(dat = dat_pred, n_fantasy_teams = 5, p = 7)

#build submission and write to file
library(dplyr)
final_teams <- res %>%
  # inner_join(team)  %>%
  arrange(name)
counts <- final_teams %>% group_by(Nickname) %>% count %>% arrange(desc(n))
counts
#format data so it fits fanduel csv format
fd_ss <- create_fanduel_lineup_spreadsheet(final_teams)

#update spreadsheet if necessary
#update_csv(fd_ss,date)
date = as.character(Sys.Date())
write.csv(fd_ss, file = paste0("./data-raw/lineup",date,".csv"), row.names = FALSE)


}
