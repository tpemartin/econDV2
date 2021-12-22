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
