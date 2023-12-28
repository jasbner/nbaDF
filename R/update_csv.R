#' Title
#'
#' @param dat
#' @param date
#'
#' @return
#' @export
#'
#' @examples
update_csv <- function(dat, date) {
  curr_entry <- list.files("./data-raw/Fanduel/", pattern = paste0("FanDuel-NBA-", date, ".*?entries-upload-template"), full.names = TRUE)
  if (length(curr_entry) > 0) {
    previous_entry <- read_csv(curr_entry, col_types = cols(.default = "c"), col_select = 1:3, name_repair = "minimal", lazy = TRUE)
    previous_entry <- previous_entry %>%
      filter(!is.na(entry_id)) %>%
      select(entry_id:contest_name) %>% filter(row_number()<=nrow(dat))

    if (nrow(previous_entry) != nrow(dat)) {
      stop("Number of rows in given dataframe does not match number of rows in template")
    }
    textfile <- read_file(curr_entry)
    previous_entry <- mutate(previous_entry, id = rownames(previous_entry))
    for (each in previous_entry$entry_id) {
      dat_index <- previous_entry |> group_by(contest_id) |>  mutate(rn = row_number()) |> filter(entry_id == each) |> pull(rn)
      textfile <- str_replace(textfile, paste0("(", each, '.*?\\)",").*?(?=,""|\\\n|$)'), paste0("\\1", paste0(dat[dat_index, ], collapse = '","'),'"'))
    }

    write_file(textfile, file = paste0("./data-raw/generated_lineups/lineup-updated-", date, ".csv"))
  }
}


