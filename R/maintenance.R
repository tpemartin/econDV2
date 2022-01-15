#' Create an package mainenance instance
#'
#' @return
#' @export
#'
#' @examples none
Maintenance <- function(){
  maintenance <- list()

  maintenance$re_install <- install_econDV2
  maintenance$fix_chineseEncoding <- fix_chineseEncoding

  return(maintenance)
}

install_econDV2 <- function(){
  devtools::install_github("tpemartin/econDV2", force=T)
}
fix_chineseEncoding <- function(){

  if(!isProject()){
    create_project()
  }
  if(isRrofile()){
    .lines <- xfun::read_utf8(".Rprofile")
    if(isProperFirstLine(.lines)){
      message("Your chinese encoding problem can not be fixed by this method.")
    } else {
      .lines <- c("options(encoding = \"UTF-8\")", .lines)
      xfun::write_utf8(.lines, ".Rprofile")
    }
  } else {
    "options(encoding = \"UTF-8\")" |>
      xfun::write_utf8(".Rprofile")
  }

  rstudioapi::openProject()

}
isRprofile <- function(){
  file.exists(".Rprofile")
}
isProperFirstLine <- function(.lines){
  .lines[[1]] == "options(encoding = \"UTF-8\")"
}
isProject <- function(){
  list.files() -> .files
  flag_Rproj <- (.files |> stringr::str_detect(".Rproj$") |> any())
  return(flag_Rproj)
}
create_project <- function(){
  cwd <- getwd()
  promptMsg <- glue::glue("This is not a project folder --\nWhat you want to do require a project folder. \nDo you want me to make the current working folder a project folder:\n{cwd}.\nInput yes or no")
  .choice <- readline(message(promptMsg))
  .choice |> tolower() -> .choice
  stringr::str_sub(.choice, 1,1) -> .choice
  if(isTRUE(.choice=="y")){
    rstudioapi::initializeProject(
      path = cwd
    )
  }
}
