#' Title
#'
#' @param date
#'
#' @import dplyr
#' @import tidyr
#'
#' @return
#' @export
#'
#' @examples
load_todays_fanduel <- function(date = as.character(Sys.Date())){
  #get file location for today's games
  curr_nba_date_dat <- list.files("./data-raw/Fanduel", pattern = paste0("FanDuel-NBA-", gsub("-",".*",date),".*?players-list"), full.names = TRUE)

  #read in nba data
  dat <- readr::read_csv(curr_nba_date_dat)
  dat <- dat |>  dplyr::rename(first = `First Name`, last = `Last Name`, injury_flag = `Injury Indicator`, injury_desc = `Injury Details`)

  #one hot encode team
  dat <- dat |>  dplyr::mutate(value = 1, Team1 = Team)  |> tidyr::pivot_wider(names_from = "Team1", values_from = "value", names_prefix = "team_", values_fill = list(value = 0))
  #one hot encode player
  dat <- dat |>  dplyr::mutate(value = 1,Player1 = Id)  |> tidyr::pivot_wider(names_from = "Player1", values_from = "value", names_prefix = "player_", values_fill = list(value = 0))

  #split position column into 2 if necessary
  dat <- dat |>  tidyr::separate_rows(Position)

  # one hot encode position
  dat <- dat |> dplyr::mutate(
    posSF = as.integer(Position == "SF"),
    posPG = as.integer(Position == "PG"),
    posPF = as.integer(Position == "PF"),
    posC = as.integer(Position == "C"),
    posSG = as.integer(Position == "SG")
  )

  dat <- dat |> dplyr::filter(!is.na(FPPG))
  dat
}
