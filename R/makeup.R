#' initiate a plot instance for ggplot
#'
#' @return
#' @export
Plot = function(){
  gg=new.env()
  gg$ggplot = NULL
  gg$geom = list()
  gg$others = list()
  gg$make = function(){
    names(gg) |>
      stringr::str_subset("geom") |>
      sort() -> geomNames
    names(gg) |> setdiff(c("ggplot",geomNames, "make"))-> components
    gglist = as.list(gg)
    gglist[c("ggplot", geomNames, components)] |> purrr::reduce(`+`)
  }
  return(gg)
}
#' Collection of ggplot personal designs
#'
#' @return
#' @export
#'
#' @examples \donotrun{
#' makeup = econDV2::Makeup()
#' plt$theme = makeup$theme()
#' }
Makeup = function(){
  makeup = new.env()
  makeup$geom_col = function(...){
    list(
      description = "Add transparency to filled colors and have 0 in y-axis touch down",
      geom = function(...) ggplot2::geom_col(..., alpha=0.5),
      scale_y_continuous =function(...) ggplot2::scale_y_continuous(..., expand=c(0,0))
    )
  }
  makeup$theme = function(){
    theme_classic()
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_line(color="#c4d1d7")
    )
  }
  return(makeup)
}
