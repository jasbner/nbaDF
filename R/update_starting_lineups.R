#' Title
#'
#' @import rvest
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
update_starting_lineups <- function(dat){
  url_today <- "https://www.rotowire.com/basketball/nba-lineups.php"
  #scrape all the links onthe page
  page <- read_html(url_today)

  name <- page |> html_elements(".lineup__player") |> html_elements("a") |> html_attr("title")
  position <- page |> html_elements(".lineup__player") |> html_elements("div") |> html_text2()


  predicted_players <- data.frame(name = name, position = position)
  predicted_players$GroupId <- cumsum(predicted_players$position == "PG")

  predicted_players <- predicted_players |> group_by(GroupId) |> filter(row_number()<6) |> ungroup()

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

  # merge players
  merged_players <- predicted_players |> left_join(players, by = c("name" = "PLAYER_NAME_simplified"))

  # throw error if we can't find a certain player
  if(nrow(merged_players |> filter(is.na(PLAYER_ID))) > 0){
    stop("Some starting players could not be found check update_starting_lineups function")
  }

  # set starters based on rotowire scraping
  dat <- dat |> mutate(starter = as.numeric(PLAYER_ID %in% merged_players$PLAYER_ID))

  dat
}
