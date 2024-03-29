% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot.R
\name{ggdash}
\alias{ggdash}
\title{Create shiny dashboard for the copied ggplot scripts}
\usage{
ggdash()
}
\description{
Any preceding lines that are necessary for the ggplot must be followed by five # sign, i.e. #####, for the program to distinguish which part is make condition, which is ggplot code.
}
\examples{
# copy the following lines then run the function.
library(ggplot2) # not require if ggplot2 is the only library you need
#####
ggplot(mtcars, aes(x = mpg)) +
geom_dotplot(
  binwidth = 1.5, #input$binwidth
  fill = "white", #input$fill
  stroke = 2 #input$stroke
  )

}
