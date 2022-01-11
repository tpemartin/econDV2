#' Enhance geom function with background color (default is white)
#'
#' @param geom a ggplot2 geom function
#' @param overlayColor default="white"
#'
#' @return an enhanced geom function
#' @export
#'
#' @examples none
geom_overlay <- function(geom,
  overlayColor="white"){
  quo_geom <- rlang::enexpr(geom)
  rlang::expr(
    list(
      do.call(
        !!quo_geom,
        argList2
      ),
      (!!quo_geom)(
        mapping=mapping,
        data=data,
        ...
      )
    )
  ) -> todo
  # browser()
  function(mapping=NULL, data=NULL, ...){
    argList2 <- argList <- list(...)
    argList2$color <- overlayColor
    argList2$colour <- NULL
    argList2$alpha <- 1
    argList2$fill <- overlayColor
    argList2 <- append(
      argList2, list(mapping=mapping, data=data)
    )
    rlang::eval_bare(
      todo
    )
  }
}
#' Generate Taiwan choropleth map to city or township level without any map supplied
#'
#' @param data A data frame with map_id to merge with underlying implicit Taiwan map sf object
#' @param map_id A character showing the name of the map_id in data input. The data[[map_id]] will be used to join with sf_taiwan_simplied$台灣本島$xxx's map_id column where xxx is determined by type input value.
#' @param ... other parameters passed to geom_sf
#' @param type either "縣市" or "鄉鎮區"
#' @param background.fill the fille of background Taiwan map
#' @param background.color the color of background Taiwan map
#' @param background.size the size of boundary of background map
#'
#' @return
#' @export
#'
#' @examples
#' ggplot() +
#'   geom_sf_taiwan(
#'     data=gg_drug$data,
#'     map_id="發生地點",
#'     mapping=aes(fill=案件次數),
#'     type="鄉鎮區"
#'   )
geom_sf_taiwan <- function(data, map_id, ...,
  type=c("縣市", "鄉鎮區"),
  background.fill= "#c8c5be",
  background.color= "#c8c5be",
  background.size= 0,
  cast2multipolygon = T
) {

  type = match.arg(type)
  .sf=sf_taiwan_simplified[["台灣本島"]][[type]]

  if(isTRUE(cast2multipolygon)){
    .sf <- sf::st_cast(.sf, "MULTIPOLYGON")
  }

  which2pick <- which(.sf$map_id %in% data[[map_id]])
  .sf[which2pick, ] |>
    dplyr::left_join(
      data,
      by=c("map_id"=map_id)
    ) -> .sf_choropleth

  list(
    geom_sf(
      data=.sf,
      fill=background.fill,
      color=background.color,
      size=background.size
    ),
    geom_sf(
      data=.sf_choropleth,
      ...
    )
  )
}
