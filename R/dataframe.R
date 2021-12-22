#' Extend a data frame with x(data/time), y/y2 (two numerics) with ymin, ymx and regime
#'
#' @description The function augement a dataset with three more columns, ymin, ymax and regime, where ymin and ymax are pmin and pmax of y and y2, regime tells y>=y2 or y<=y2 happens. In addition, interpolated x observations are inserted to prevent void area in geom_ribbon usage when x is discreate
#' @param .df a data frame to select x, y and y2 from, where x must have "POSIXct" "POSIXt" class, y and y2 are numeric
#' @param targetColumns a named character as c("x"="x in .df", "y"="y in .df", "y2"="y2 in .df") where "zzz in .df" means column name in .df that represents zzz vector for switching regime dataset construction.
#'
#' @return
#' @export
#'
#' @examples targetColumns = c("x"="year1",
#'   "y"="商品及服務輸出佔比", "y2"="商品及服務輸入佔比")
#'   get_switching_regime_dataframe(.df, targetColumns)
get_switching_regime_dataframe <- function(.df, targetColumns) {
  .df[,targetColumns] ->
    .df_selected
  names(.df_selected) <- names(targetColumns)
  .df_selected |>
    econDV2::find_switchingPoint() -> switchingPoints

  switchingPoints$y2 <- switchingPoints$y
  dplyr::bind_rows(
    .df_selected,
    switchingPoints
  ) |>
    dplyr::arrange(x) ->
    .df_selected_switching

  .df_selected_switching |>
    mutate(
      ymin = pmin(y,y2),
      ymax = pmax(y,y2)
    ) -> .df_selected_switching
  .df_selected_switching |>
    dplyr::filter(
      y>=y2
    ) |>
    mutate(
      regime="regime1")-> regime1
  .df_selected_switching |>
    dplyr::filter(
      y<=y2
    ) |>
    mutate(
      regime="regime2")-> regime2

  .df_switching_regimes <-
    dplyr::bind_rows(
      regime1, regime2
    )
  return(.df_switching_regimes)
}
