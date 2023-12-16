#' Title
#'
#' @return
#' @export
#'
#' @import lubridate
#' @import dplyr
#' @import httr2
#' @import purrr
#'
#' @examples
update_games_details <- function(){
  # games_details -----------------------------------------------------------
  data(games)
  data(games_details)
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

  if(nrow(new_games)>0){
    # add 00 to front of GAME_ID to fit format of stats
    urls = paste0('https://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=0&GameID=00',new_games_str,'&RangeType=0&StartPeriod=1&StartRange=0')


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

  games_details

}
