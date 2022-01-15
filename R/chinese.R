#' Generate map_id_en column for a data frame
#'
#' @param df a data frame
#' @param map_id a character of a column in df that represents chinese map_id
#'
#' @return a data frame with extra map_id_en column
#' @export
#'
#' @examples none
generateColumn_map_id_en <- function(df, map_id) {
  require(sf)
  by = "欄位2"
  names(by) <- map_id
  df |>
    dplyr::left_join(
      postal_codes_new |>
        dplyr::select("欄位2", "map_id_en"), by=by
    ) -> .sf
  return(.sf)
}

# postal_codes |>
#   generate_map_id_en_for_postal_codes_df() -> postal_codes_new
# sf_taiwan_simplified$鄉鎮區 |>
#   generateColumn_map_id_en(map_id="map_id", postal_codes_new = postal_codes_new) |> View()

generate_map_id_en_for_postal_codes_df <- function(postal_codes){
  postal_codes |>
    dplyr::mutate(
      map_id_en=generate_map_id_en(欄位3)
    )
}

generate_map_id_en <- function(.col){
  .col |>
    stringr::str_replace(" [a-zA-Z]+.?, ", "_") -> col3_stage1
  col3_stage1 |>
    stringr::str_remove("’") -> col3_stage2
  col3_stage2 |>
    stringr::str_replace_all("\\s", "_") -> col3_stage3
  return(col3_stage3)
}
# purrr::map(
#   raw_ids,
#   ~{
#     .x[[1]] |>
#       get_1st_name_raw_idsXY() -> name1
#     .x[[2]] |>
#       get_1st_name_raw_idsXY() -> name2
#     c(name1, name2)
#   }
# ) -> list_dist_city

get_1st_name_raw_idsXY <- function(raw_idsXY){
  raw_idsXY |>
    stringr::str_split("\\s+") ->
    name1
  name1b <- name1[[1]] |> stringr::str_remove("\\s+")
  name1b |> subset(name1b!="") -> name1c
  name1c[[1]]
}
