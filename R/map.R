#' Construct map object for various map related tools
#'
#' @return
#' @export
#'
#' @examples
Map <- function(){
  require(ggmap)
  map <- new.env()
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

  map$choropleth$rename_valueData_countryname <- rename_countries
  map$scales$scale_fill_ordered_factor <- scale_fill_ordered_factor
  map$choose_palette <- function(){
    colorspace::choose_palette(gui="shiny") ->
      map$palettte
  }
  map$convert_colorHex2hsl <- function(colors){
    colors <- tolower(colors)
    farver::decode_colour(colors, alpha = TRUE, to = "hsl",
      na_value = "transparent")
  }
  map$extract$googleMapLocation <-  extract_googleMapLocation
  map$extract$osmBBox <- extract_osmBBox
  map$osm$request_data <- osm_request_data
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
rename_countries <- function(df_wdi2020, countryColumnName, pattern) {
  country0 <- pattern
  country1 <- names(pattern)
  for(.x in seq_along(country0)){
    whichIs <- which(df_wdi2020[[countryColumnName]] == country0[[.x]])

    if(length(whichIs)==0){
      next
    }
    df_wdi2020[
      whichIs,
    ][[countryColumnName]] <- country1[[.x]]
  }
  df_wdi2020
}

scale_fill_ordered_factor <- function(.ordered_fct, low, high, na.value="#919191") {
  color_values <- {
    scales::colour_ramp(
      c(low, high)
    ) -> binPalette
    scales::show_col(
      binPalette(c(0, 0.33, 0.66, 1))
    )
    binPalette(
      seq(from=0, to=1, length.out=length(levels(.ordered_fct)))
    )
  }
  scale_fill_manual(
    limits = c(NA, levels(.ordered_fct)),
    values = c(na.value, color_values)
  )
}
extract_osmBBox <- function(){
  bbox <- clipr::read_clip()
  bbox |> stringr::str_subset("[.0-9]+") |>
    setNames(
      c("top","left", "right", "bottom")
    ) -> bbox
  glue::glue('bbox = c(left = {bbox[["left"]]}, bottom = {bbox[["bottom"]]}, right = {bbox[["right"]]}, top = {bbox[["top"]]})') |> clipr::write_clip()

}
extract_googleMapLocation <- function(){
  clipr::read_clip()-> locationzoom
  locationzoom |>
    stringr::str_extract_all("[.0-9]+") |> unlist() |>
    as.numeric() |>
    setNames(
      c("lat", "long", "zoom")
    ) -> loc
  glue::glue('center = c(long={loc[["long"]]}, lat={loc["lat"]}),
  zoom = {loc["zoom"]}') |> clipr::write_clip()
}
osm_request_data <- function(bbox, features) {
  request <- osmdata::opq(bbox)
  for(.x in seq_along(features)){
    # .x=1
    featureX = features[.x]
    keyX=names(featureX)
    valueX=featureX
    request |> osmdata::add_osm_feature(key=keyX, value=valueX) ->
      request
  }
  request |> osmdata::osmdata_sf() -> sf_data
  return(sf_data)
}
