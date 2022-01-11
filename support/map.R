#' Construct map object for various map related tools
#'
#' @return
#' @export
#'
#' @examples
Map <- function(){
  library(ggmap)
  map <- list()
  map$browse$google <- function(){
    browseURL("https://www.google.com/maps")
  }
  map$browse$stamen <- function(){
    browseURL("http://maps.stamen.com/")
  }
  map$browse$openstreetmap <- function(){
    browseURL("https://www.openstreetmap.org/")
  }
  map$copy_paste$google_location_zoom <- generate_google_locationZoom
  map$copy_paste$osm_bbox <- generate_bbox
  
  map$prepare_dataframe_from$map <- prepareDF4geom
  
  return(map)
}
generate_google_locationZoom <- function(){
  clipr::read_clip() -> .gps
  stringr::str_split(.gps, ",") -> .str_gps
  zoom = .str_gps[[1]][[3]] |>
    stringr::str_remove("z")
  glue::glue(
    "c(lon={.str_gps[[1]][[2]]}, lat={.str_gps[[1]][[1]]}),\nzoom={zoom}"
  ) |> clipr::write_clip()
}
generate_bbox <- function()
{
  clipr::read_clip() -> bbox
  bbox |> stringr::str_subset("[0-9\\.]+") -> bbox
  #left/bottom/right/top 
  bbox[c(2, 4, 3, 1)] |> 
    setNames(
      c("left", "bottom", "right", "top")
    ) -> bbox
  paste(names(bbox), "=", bbox,  "", sep="", collapse = ",\n") |> clipr::write_clip()
}
prepareDF4geom <- function(map){
  
  map[c("x","y")] |> 
    data.frame() -> .df
  
  .df$group="taiwan"
  .df$subgroup <- {
    whichIsNa <- which(is.na(.df$x))
    .subgroup = cut(1:nrow(.df), c(0, whichIsNa, Inf))
    levels(.subgroup) <- map$names
    .subgroup
  }
  .df |> na.omit()
}
