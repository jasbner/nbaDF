# read in player data
games_details <- readr::read_csv("./data-raw/games_details.csv")
# save data to rdata file
usethis::use_data(games_details)

# read in game data
games <- readr::read_csv("./data-raw/games.csv")
# save data to rdata file
usethis::use_data(games)



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



# teams -------------------------------------------------------------------


# read in player data
team_details <- readr::read_csv("./data-raw/teams.csv")
# save data to rdata file
usethis::use_data(team_details)
