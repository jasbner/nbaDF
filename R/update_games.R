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
update_games<- function(){
  # now we need to fill the gaps in games
  # get array of dates since the last game in the our stored dataset
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

  games
}
