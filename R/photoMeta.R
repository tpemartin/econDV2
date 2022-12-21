#' Extract File metat data using EXIF
#'
#' @description Need to install EXIFtools first from https://exiftool.org/
#'
#' @param files a vector of file paths
#' @param gpsOnly default = TRUE
#' @param convertGPS default = TRUE, convert GPS from degree-minute-second format to decimal format
#'
#' @return a dataframe, unless gpsOnly = FALSE
#' @export
#'
#' @examples
#' \dontrun{
#'   list.files("./photos", full.names = T) |>
#'   getMetaDataFromFiles(gpsOnly = T) -> dfMeta
#' }
getMetaDataFromFiles = function(files, gpsOnly=TRUE, convertGPS=TRUE){
  files |>
    purrr::map(
      ~{getFileMetaData(.x, gpsOnly)}
    ) -> dfMeta
  if(gpsOnly){
    dfMeta |>
      purrr::transpose() |>
      purrr::map(unlist) |>
      as.data.frame() -> dfMeta
    if(convertGPS){
      dfMeta = convertDfMetaGPS(dfMeta)
      dfMeta$filename = files
    }
  } else {
    names(dfMeta) = files
  }
  return(dfMeta)
}

# helpers -----

extractOnlyGPS = function(result) {
  result |>
    stringr::str_subset("GPS") -> gpsComponents
  gpsComponents |>
    stringr::str_subset("^GPS\\s+(Altitude|Latitude|Longitude)\\s+:") -> gpsCompoents1
  gpsCompoents1 |>
    stringr::str_extract("^GPS\\s+(Altitude|Latitude|Longitude)\\s+:") |>
    stringr::str_remove_all("GPS|\\s+|:") -> itemNames
  gpsCompoents1 |>
    stringr::str_remove("^GPS\\s+(Altitude|Latitude|Longitude)\\s+:\\s") ->
    itemValues
  itemValues |>
    as.list() |>
    setNames(itemNames)
}
getFileMetaData = function(filename = "/Users/martin/Downloads/IMG_0195.HEIC", gpsOnly=TRUE){
  # install exiftool from
  #. https://exiftool.org/
  result = system(
    glue::glue("exiftool '{filename}'"), intern=TRUE)
  if(gpsOnly) result <- extractOnlyGPS(result)
  return(result)
}
convertGPS2Decimal <- function(x) {
  x |> stringr::str_extract_all("[0-9\\.]+") |>
    purrr::map_dbl(
      ~{
        .x = as.numeric(.x)
        .x[[1]]+.x[[2]]/60+.x[[3]]/3600
      }
    )
}
convertDfMetaGPS = function(dfMeta) {
  dfMeta |>
    dplyr::mutate(
      Latitude = convertGPS2Decimal(Latitude),
      Longitude = convertGPS2Decimal(Longitude)
    )
}
