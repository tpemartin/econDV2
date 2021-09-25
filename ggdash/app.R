#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rclipboard)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    rclipboard::rclipboardSetup(),

    # Application title
    titlePanel("GG Dash, by NTPU Economics Department"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            shiny::numericInput("binwidth","binwidth", 1.5),
            shiny::textInput("fill","fill", "white"),
            shiny::numericInput("stroke","stroke", 2),
            uiOutput("clip")

        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("ggexperiment"),
            shiny::textOutput("text")
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    library(ggplot2)
    .plotScript <- "ggplot(mtcars,aes(x=mpg))+\ngeom_dotplot(\nbinwidth=input$binwidth,#input$binwidth\nfill=input$fill,#input$fill\nstroke=input$stroke#input$stroke\n)"
    generate_copyText <- function(serverText, ...) {
        inputs <- list(...)
        patterns <- paste0("input\\$", names(inputs))
        replacements <- create_replacementText(inputs)
        names(replacements) <- patterns
        serverText2 <- stringr::str_remove_all(serverText, "#input\\$.+(?=\\\\\\n)")
        stringr::str_replace_all(serverText2, replacements)
    }
    create_replacementText <- function(inputs) {
        purrr::map_chr(inputs, ~{
            if (is.character(.x)) {
                paste0("\"", .x, "\"")
            }
            else {
                as.character(.x)
            }
        })
    }
    output$ggexperiment <- renderPlot({
        ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = input$binwidth,
            fill = input$fill, stroke = input$stroke)
    })
    updatedPlotScript <- reactive({
        generate_copyText(.plotScript,
            binwidth = input$binwidth, fill = input$fill, stroke = input$stroke)
    })
    output$text <- renderText({
        updatedPlotScript()
    })
    output$clip <- renderUI({
        rclipboard::rclipButton("clipbtn", "Copy",
            updatedPlotScript(),
            icon("clipboard"))
    })
}
# {
#         library(ggplot2)
#
#         .plotScript <- 'ggplot(mtcars,aes(x=mpg))+
# geom_dotplot(
# binwidth=input$binwidth,#input$binwidth
# fill=input$fill,#input$fill
# stroke=input$stroke#input$stroke
# )'
#         generate_copyText <- function(serverText, ...){
#             inputs <- list(...)
#             patterns <- paste0("input\\$", names(inputs))
#             replacements <- create_replacementText(inputs)
#             names(replacements) <- patterns
#             serverText2 <- stringr::str_remove_all(
#                 serverText, "#input\\$.+(?=\\\\\\n)"
#             )
#             stringr::str_replace_all(
#                 serverText2, replacements
#             )
#         }
#         create_replacementText <- function(inputs){
#             purrr::map_chr(
#                 inputs,
#                 ~{
#                     if(is.character(.x)){
#                         paste0('"', .x, '"')
#                     } else {
#                         as.character(.x)
#                     }
#                 })
#         }
#         output$ggexperiment <- renderPlot({
#             ggplot(mtcars,aes(x=mpg))+
#                 geom_dotplot(
#                     binwidth=input$binwidth,#input$binwidth
#                     fill=input$fill,#input$fill
#                     stroke=input$stroke#input$stroke
#                 )
#         })
#         # Add clipboard buttons
#         output$clip <- renderUI({
#             rclipboard::rclipButton("clipbtn", "rclipButton Copy",
#                 generate_copyText(.plotScript,
#                     binwidth = input$binwidth,
#                     fill = input$fill,
#                     stroke = input$stroke),
#                 # .plotScript,
#                 icon("clipboard"))
#         })
#
#     ###
#     library(ggplot2)
#     # .plotScript <- "ggplot(mtcars,aes(x=mpg))+\ngeom_dotplot(\nbinwidth=input$binwidth,#input$binwidth\nfill=input$fill,#input$fill\nstroke=input$stroke#input$stroke\n)"
#     # generate_copyText <- function(serverText, ...){
#     #     inputs <- list(...)
#     #     patterns <- paste0("input\\$", names(inputs))
#     #     replacements <- create_replacementText(inputs)
#     #     names(replacements) <- patterns
#     #     serverText2 <- stringr::str_remove_all(
#     #         serverText, "#input\\$.+(?=\\\\\\n)"
#     #     )
#     #     stringr::str_replace_all(
#     #         serverText2, replacements
#     #     )
#     # }
#     # create_replacementText <- function(inputs){
#     #     purrr::map_chr(
#     #         inputs,
#     #         ~{
#     #             if(is.character(.x)){
#     #                 paste0('"', .x, '"')
#     #             } else {
#     #                 as.character(.x)
#     #             }
#     #         })
#     # }
#     # output$ggexperiment <- renderPlot({
#     #     library(ggplot2)
#     #     output$ggexperiment <- renderPlot({
#     #         ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = input$binwidth,
#     #             fill = input$fill, stroke = input$stroke)
#     #     })
#     # })
#     output$text <- renderText(
#         generate_copyText(.plotScript,
#             binwidth = input$binwidth,
#             fill = input$fill, stroke = input$stroke)
#     )
#     # Add clipboard buttons
#     # output$clip <- renderUI({
#     #     rclipboard::rclipButton("clipbtn", "rclipButton Copy",
#     #         generate_copyText(.plotScript,
#     #             binwidth = input$binwidth,
#     #             fill = input$fill, stroke = input$stroke),
#     #         # .plotScript,
#     #         icon("clipboard"))
#     # })
# }

# Run the application
shinyApp(ui = ui, server = server)
