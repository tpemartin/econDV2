#' Attach Plot constructor
#'
#' @return none.
#' @export
attachPlot = function(){
  pltenv = new.env()
  pltenv$Plot=Plot
  attach(pltenv)
}
