#' Create shiny dashboard for the copied ggplot scripts
#'
#' @description Any preceding lines that are necessary for the ggplot must be followed by five # sign, i.e. #####, for the program to distinguish which part is make condition, which is ggplot code.
#' @return
#' @export
#'
#' @examples # copy the following lines then run the function.
#' library(ggplot2) # not require if ggplot2 is the only library you need
#' #####
#' ggplot(mtcars, aes(x = mpg)) +
#' geom_dotplot(
#'   binwidth = 1.5, #input$binwidth
#'   fill = "white", #input$fill
#'   stroke = 2 #input$stroke
#'   )
#'
ggdash <- function(){
  plotcopy <- ggcopy()
  require(magrittr)
  inputs <- {
    stringr::str_remove_all(
      plotcopy, "\\s"
    ) %>%
    stringr::str_extract_all("(?<=#)input\\$.+") %>%
      unlist()
  }
  input_names <- {
    purrr::map_chr(
      inputs,
      ~{
        stringr::str_extract(.x, "(?<=\\$).*")
      }
    )
  }
  inputValues <- {
    stringr::str_extract_all(
      plotcopy, "[^=\\(,]*(?=\\s*,?\\s*#\\s*input)"
    ) %>%
      purrr::map(
        ~{stringr::str_remove_all(.x, "\\s") -> .x2
          subset(.x2, .x2!="")}
      ) %>%
      unlist()
  }
  inputValueIsText <- {
    stringr::str_detect(
      inputValues,
      #inputValues[[5]][[1]],
      "[\"']")
  }
  uiInputTags <- {
    get_UItags(input_names, inputValues, inputValueIsText)
  }
  uiScript <- {
    get_uiText(uiInputTags)
  }
  serverText <- {
    get_serverText(plotcopy, inputs, input_names)
  }
  serverScript <- {
    get_serverScript(serverText)
  }
  runGGDash(uiScript, serverScript)
}


# helpers -----------------------------------------------------------------

runGGDash <- function(uiScript, serverScript)
{
  uiScript %>%
    str2expression() %>%
    eval(envir = .GlobalEnv)
  serverScript %>%
    str2expression() %>%
    eval(envir = .GlobalEnv)
  shiny::shinyApp(ui = ui, server = server,
    options=list(
      launch.browser=T
    ))
  # runAppStr <- "shiny::shinyApp(ui = ui, server = server)"
  # eval(parse(text=runAppStr), .GlobalEnv)
}
get_serverScript <- function(serverText){
  stringr::str_which(
    serverText, "#####"
  ) -> endOfMakecondition
  makecondition=""
  serverText = serverText
  if(length(endOfMakecondition)!=0){
    makecondition <- serverText[1:endOfMakecondition]
    serverText <- serverText[-c(1:endOfMakecondition)]
  }
  serverPhrase(
    collapse(makecondition),
    collapse(serverText)) -> serverScript
  return(serverScript)
}

collapse <- function(str){
  paste0(str, collapse="\n")
}

get_serverText <- function(plotcopy, inputs, input_names){
  plotcopy %>%
    stringr::str_remove_all("\\s") -> str_spaceRemoved


  for(.x in seq_along(inputs))
  {
    .replacement = inputs[[.x]]
    .pattern = glue::glue("[^\\=\\(,]+(?=,?#input\\${input_names[[.x]]})")
    str_spaceRemoved %>%
      stringr::str_replace(.pattern, .replacement) -> str_spaceRemoved
  }
  return(str_spaceRemoved)
}

sequentialReplaceValueWithInputVerbatim_function <-
  function(inputs, input_names){
    function(rScript, .x){
      .replacement = inputs[[.x]]
      .pattern = glue::glue("[^\\=\\(,]+(?=,?#input\\${input_names[[.x]]})")
      str_spaceRemoved %>%
        stringr::str_replace(.pattern, .replacement) -> updated_rScript

      updated_rScript
    }
  }
get_uiText <- function(uiInputTags){
  uiInputTags_refined <- refine_then_collapse_uiTags(uiInputTags)
  uiText <- uiPhrase(uiInputTags_refined)
  return(uiText)
}
refine_then_collapse_vector <- refine_then_collapse_uiTags <- function(uiInputTags){
  c(
    paste0(uiInputTags[1:(length(uiInputTags)-1)], ","),
    uiInputTags[[length(uiInputTags)]]
  ) %>%
    paste0(collapse = "\n") -> uiInputTags_refined
  return(uiInputTags_refined)
}

get_UItags <- function(input_names, inputValues, inputValueIsText){
  purrr::pmap(
    list(input_names, inputValues, inputValueIsText),
    get_inputTag
  ) %>%
    unlist()
}
get_inputTag <- function(name, value, isText){
  if(isText){
    inputTag <- glue::glue('shiny::textInput("{name}","{name}", {value})')
  } else {
    inputTag <- glue::glue('shiny::numericInput("{name}","{name}", {value})')
  }
  inputTag
}
uiPhrase <- function(uiInputTags){
  glue::glue('library(shiny)
  ui <- fluidPage(

    # Application title
    titlePanel("GG Dash, by NTPU Economics Department"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        {uiInputTags}
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("ggexperiment")
      )
    )
  )')
}
serverPhrase <- function(makecondition,serverText){
  glue::glue('server <- function(input, output) {
  library(ggplot2)
    <<makecondition>>
    output$ggexperiment <- renderPlot({
  <<serverText>>
    })
}
', .open="<<", .close=">>")
}

ggcopy <- function() clipr::read_clip()
