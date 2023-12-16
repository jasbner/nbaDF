#' Title
#'
#' @param dat
#'
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
create_fanduel_lineup_spreadsheet <- function(dat){
  dat <- dat |>  select(Id,name,Position)
  wider <- dat |>  pivot_wider(names_from = "Position", values_from = "Id",values_fn = list(Id = list))
  wider <- wider |>  select(name,PG,SG,SF,PF,C)
  wider <- wider |>  rowwise() |>   mutate(PG1 = PG[1],PG = PG[2],SG1 = SG[1],SG = SG[2],SF1 = SF[1],SF= SF[2],PF1 = PF[1],PF = PF[2],C = C[1])
  wider <- wider |>  select(starts_with("PG"),starts_with("SG"),starts_with("SF"),starts_with("PF"),"C")
  names(wider) <- c("PG","PG","SG","SG","SF","SF","PF","PF","C")
  wider
}
