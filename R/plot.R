Plot <- function(data) {
  self = new.env()
  self$data=data
  self$ggplot=NULL
  self$geoms=NULL
  self$make=function(){
    self$ggplot+self$geoms
  }
  self$save=function(){
    saveRDS(self, filename)
    message(paste("The plot is saved at ", filename))
  }
  return(self)
}

#' Attach Plot constructor
#'
#' @return none.
#' @export
attachPlot = function(){
  pltenv = new.env()
  pltenv$Plot=Plot
  attach(pltenv)
}
