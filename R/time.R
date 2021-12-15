#' Get switching points data frame from a data frame with x (data/time), y and y2 (for switching comparison)
#'
#' @param .df a data frame
#'
#' @return a data frame of switching date-time and intersect y value
#' @export
#'
#' @examples none
find_switchingPoint <- function(.df){
  .preSwitchingDates <-  
    get_preswitchingDates(.df)
  purrr::map_dfr(
    .preSwitchingDates,
    ~intersect_datetimeX_numericY(.df, .x)
  )
}
library(lubridate)
intersect_datetimeX_numericY <- function(.df, x0) {
  dx=dx
  x1=x0+dx
  whichIsPreswitching <- (.df$x == x0) |> which()
  whichIsPreswitching
  .df[c(0,1)+whichIsPreswitching, ] -> .dfX
  .dfX$x |> lubridate::as_datetime() |> as.double() -> timeInDouble
  ab1=get_ab(timeInDouble, .dfX$y)  
  ab2=get_ab(timeInDouble, .dfX$y2)
  solution_x <- (ab1[["a"]]-ab2[["a"]])/(ab2[["b"]]-ab1[["b"]])
  solution_y <- ab1[["a"]]+ab1[["b"]]*solution_x
  solution_y
  data.frame(
    x=lubridate::as_datetime(solution_x),
    y=solution_y)
}

get_ab <- function(x,y){
  x0=x[[1]]; xend=x[[2]]
  y0=y[[1]]; yend=y[[2]]
  dx=xend - x0
  dy=yend - y0
  slope=dy/dx
  a=y0-slope*x0
  b=slope
  c(a=a, b=b)
}
get_preswitchingDates <- function(.df){
  .df |> 
    mutate(
      lead_y=dplyr::lead(y),
      lead_y2=dplyr::lead(y2),
      preSwitching={
        (y>=y2 & lead_y < lead_y2) |
          (y<y2 & lead_y >= lead_y2)
      }
    ) |>
    dplyr::filter(
      preSwitching
    ) |>
    pull(x) -> .preSwitchingDates
  .preSwitchingDates
}
