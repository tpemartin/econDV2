#' A fix to ggmap function that put overly align
#'
#' @param map A map object obtained from ggmap::get_...map()
#'
#' @return a gg object
#' @export
#'
#' @examples none
ggmap2 <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
    c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  ggmap::ggmap(map)
}
#' A enhanced geom_sf for overlaying a gg object produced by ggmap2
#'
#' @param data a sf data object.
#' @param ... others that pass to regular geom_sf
#'
#' @return
#' @export
#'
#' @examples none.
geom_sf_overggmap <- function(data=data, ...){
  list(
    ggplot2::geom_sf(
      data=sf::st_transform(data, 3857),
      ...
    ),
    ggplot2::coord_sf(crs = sf::st_crs(3857))
  )
}
