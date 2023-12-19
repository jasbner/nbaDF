update_injury_report <- function(){
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


  #get current webpage
  url_today <- "https://official.nba.com/nba-injury-report-2023-24-season/"

  #scrape all the links onthe page
  library(rvest)
  page <- read_html(url_today)
  links <- page %>% html_nodes("a") %>% html_attr("href")
  links <- links[!is.na(links)]
  links <- links[str_detect(links,"Injury-Report")]
  #take the last in the list
  recent_report <- links[length(links)]
  file_1 <- tempfile()
  file_1

  download.file(recent_report,file_1)

  # files <- list.files("data-raw/injury_reports",pattern = "pdf$", full.names = TRUE)
  res_df <- parse_injury_report(pdf_text(file_1))

  res_df$report_date <- recent_report |> str_extract("\\d+-\\d+-\\d+") |> as_date()


  res_df
}
