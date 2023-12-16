#' Title
#'
#' @param dat
#'
#' @return
#' @export
#' @import ompr
#' @import ompr.roi
#' @import ROI.plugin.glpk
#'
#' @examples
optim_nba <- function(dat, total_salary = 60000, n_fantasy_teams = 1, p = 8) {
  # functions to determine if a particular player is of a position
  get_posSF_val <- function(rownum) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::pull(posSF)
  }
  get_posPG_val <- function(rownum) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::pull(posPG)
  }
  get_posPF_val <- function(rownum) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::pull(posPF)
  }
  get_posC_val <- function(rownum) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::pull(posC)
  }
  get_posSG_val <- function(rownum) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::pull(posSG)
  }


  # need to update this to get expected points from projection website
  get_pts <- function(rownum) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::pull(FPPG)
  }

  get_sal <- function(rownum) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::pull(Salary)
  }

  get_injury <- function(rownum) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::mutate(inj = as.integer(ifelse(injury_flag == "GTD" | is.na(injury_flag), 0, 1))) |>
      dplyr::pull(inj)
  }


  get_min <- function(rownum) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::mutate(min_minutes = as.integer(is.na(mpgSeason))) |>
      dplyr::pull(min_minutes)
  }

  get_one <- function(rownum) {
    1
  }

  # get number of players
  n <- nrow(dat)

  model <- MIPModel() |>
    add_variable(x[i], i = 1:n, type = "binary") |>
    # set objective (maximize points)
    set_objective(sum_expr(colwise(get_pts(i)) * x[i], i = 1:n),sense = "max") |>
    add_constraint(sum_expr(colwise(get_sal(i)) * x[i], i = 1:n) <= total_salary) |>
    add_constraint(sum_expr(colwise(get_one(i)) * x[i], i = 1:n) == 9) |> #9 total players
    add_constraint(sum_expr(colwise(get_posPG_val(i)) * x[i], i = 1:n) >= 2) |>
    add_constraint(sum_expr(colwise(get_posSG_val(i)) * x[i], i = 1:n) >= 2) |>
    add_constraint(sum_expr(colwise(get_posSF_val(i)) * x[i], i = 1:n) >= 2) |>
    add_constraint(sum_expr(colwise(get_posPF_val(i)) * x[i], i = 1:n) >= 2) |>
    add_constraint(sum_expr(colwise(get_posC_val(i)) * x[i], i = 1:n) >= 1) |>
    # add injury constraint
    add_constraint(sum_expr(colwise(get_injury(i)) * x[i], i = 1:n) == 0) # |>
  # add_constraint(sum_expr(colwise(get_min(i)) * x[i], i = 1:n) == 0)

  get_team_name_val <- function(rownum, teamname) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::pull(teamname)
  }

  team_cols <- colnames(dat)[grepl("team_",colnames(dat))]
  for( t in team_cols){
    #max from each team is 4
    model <- model |> add_constraint(sum_expr(colwise(get_team_name_val(i, t)) * x[i], i = 1:n) <= 4)
  }

  get_player_name_val <- function(rownum, playername) {
    dat |>
      dplyr::filter(dplyr::row_number() == rownum) |>
      dplyr::pull(playername)
  }

  player_cols <- colnames(dat)[grepl("player_\\d",colnames(dat))]

  for( u in player_cols){
    #max from each player is 1
    model <- model |> add_constraint(sum_expr(colwise(get_player_name_val(i, u)) * x[i], i = 1:n) <= 1)
  }

  result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE)) |> get_solution(x[i])
  result_index <- result |> filter(value == 1) |> pull(i)
  res <- dat |>
    dplyr::filter(dplyr::row_number() %in% (result_index)) |>
    arrange(Position)


  get_team_val <- function(rownum, teamnum) {
    p_id <- dat |> dplyr::filter(dplyr::row_number() == rownum) |> dplyr::pull(Id)
    out |>
      dplyr::filter(Id == p_id) |>
      dplyr::pull(teamnum) |>
      sum()
  }

  #add first team to dat
  j <- "t1"
  out <- dat |> dplyr::mutate(!!j := as.integer(dplyr::row_number() %in% (result_index)))
  if(n_fantasy_teams>1){
    for (old_team in 1:(n_fantasy_teams-1)) {
      # colname
      j <- paste0("t", old_team)
      # add constraint that we cant have an old team exactly
      model <- model |> add_constraint(sum_expr(colwise(get_team_val(i, j)) * x[i], i = 1:n) <= p)
      # solve new model
      result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))  |> get_solution(x[i])
      # get result set
      result_ind <- result |> filter(value == 1) |> pull(i)
      res <- out |>
        dplyr::filter(dplyr::row_number() %in% (result_ind)) |>
        arrange(Position)
      # add new column to out of old team
      j <- paste0("t", old_team+1)
      out <- out |> dplyr::mutate(!!j := as.integer(dplyr::row_number() %in% (result_ind)))
    }
  }

  #browser()
  # get long format team
  res_teams <- tidyr::pivot_longer(out, cols = matches("t\\d+")) |> arrange(name) |> dplyr::filter(value == 1)
  res_teams
  #
  # # save lineups for future reference
  # fname <- paste0("lineup_", gsub("-", "_", Sys.Date()), "_", m, ".rds")
  # fname_path <- paste0("./archive/", fname)
  # saveRDS(res_teams, fname_path)
}
