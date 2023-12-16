#' Title
#'
#' @import dplyr
#' @return
#' @export
#'
#' @examples
load_training_data <- function(){
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

  game_stats
}
