#' Create an instance to deal with photo files that has getGPS method to extract GPS locations of photos
#'
#' @param path a character of either local path or an url linked to a Google drive folder.
#'
#' @return an instance that has getGPS method.
#' @export
#'
#' @examples
#' \donntrun{
#' # If supplied a Google Drive folder url:
#' folderUrl = "https://drive.google.com/drive/u/0/folders/1ldcrE5XGgN96qImQ7oYslTQCG2ccohNd"
#' folderUrl |> Photos() -> ph
#' ph$getGPS() -> dfMeta
#' # or
#' ph$getGPS(deauth=F) -> dfMeta # if require Google authentication.
#'
#' # If supplied a local path (must install exiftool first, may only work for MacOS)
#' folderPath = "./photos"
#' folderPath |> Photos() -> ph2
#' ph2$getGPS() -> dfMeta2
#' }
Photos = function(path){
  photos = list()
  photos$path = path
  photos$getGPS = construct_getGPS(path)

  return(photos)
}
#' Get photo GPS from photo files sitting inside a Google Drive folder
#'
#' @param folderUrl the url of a google drive folder that has photo files.
#' @param deauth default=T, meaning no need for google authentication.
#'
#' @return
#' @export
#'
#' @examples
#' \donntrun{
#' folderUrl = "https://drive.google.com/drive/u/0/folders/1ldcrE5XGgN96qImQ7oYslTQCG2ccohNd"
#' folderUrl |>
#'   getPhotoGPSfromGoogleDriveFolderUrl()
#' }
getPhotoGPSfromGoogleDriveFolderUrl <- function(folderUrl, deauth=T) {
  folderUrl |>
    getDribbleFromFolderUrl(deauth) |>
    getLocationsFromDribble() -> dfLocationMeta
  return(dfLocationMeta)
}

# helpers ----

construct_getGPS = function(path){

  flag_isUrl = stringr::str_detect(path, "^https://")
  if(flag_isUrl){
    return(function(deauth=T){
      path |> getPhotoGPSfromGoogleDriveFolderUrl(deauth)
    })
  } else {
    return(function(gpsOnly=T, convertGPS=T){
      path |>
        list.files(full.names = T) |>
        getMetaDataFromFiles(gpsOnly=gpsOnly, convertGPS=convertGPS)
    })
  }
}


getDribbleFromFolderUrl <- function(folderUrl, deauth) {
  if(deauth){
    googledrive::drive_deauth()
    } else {
    googledrive::drive_auth()
  }
  googledrive::as_dribble(folderUrl) ->
      drb
  return(drb)
}

getLocationsFromDribble <- function(drb) {
  googledrive::drive_ls(drb) -> listFiles
  listFiles$drive_resource |>
    purrr::map_dfr(
      ~{
        .x$imageMediaMetadata$location |>
          as.data.frame()
      }
    ) -> dfMeta
  dfMeta$filename = listFiles$name
  return(dfMeta)
}




