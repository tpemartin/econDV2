#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            shiny::numericInput("size","size", 2),
            shiny::textInput("color","color", 'pink')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ggexperiment")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(ggplot2)


    output$ggexperiment <- renderPlot({
        # generate bins based on input$bins from ui.R
        p <- ggplot(mtcars, aes(wt, mpg))+
            geom_point(
                size = input$size,
                color = input$color
            )
        p
    })
}

# Run the application
shinyApp(ui = ui, server = server)
