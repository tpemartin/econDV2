#' Extract cities and townships from addresses
#'
#' @param address a character vector of addresses starting from city, then township
#'
#' @return a data frame of city, township, and address
#' @export
extract_city_township <- function( address) {

  address |>
    stringr::str_extract("[\u4E00-\u9FFF]+") -> addresses
  addresses |>
    stringr::str_replace_all("台","臺") |>
    stringr::str_replace_all("镇","鎮")-> addresses
  addresses |>
    stringr::str_sub(1,3) -> cities
  addresses
  addresses |>
    stringr::str_sub(4) -> addressesShorten
  addressesShorten
  addressesShorten |>
    stringr::str_extract(
      "[^鄉市區]+[鄉鎮市區](?!區)"
    ) -> townships
  data.frame(
    city=cities,
    township=townships,
    address = addresses
  )
}
create_project_if_necessary <- function(){
  projectFolder <- rstudioapi::getActiveProject()
  if(is.null(projectFolder)){
    rstudioapi::showDialog(
      "Alert",
      "The folder of this directory is not a project yet. You need to make the folder a project folder. \nBe aware that everything in that folder will be used for R. You can select a different folder after you click OK."
    )
    projectFolder =
      rstudioapi::selectDirectory(
        path=getwd()
      )
    # ask set up project
    rstudioapi::initializeProject(
      path=projectFolder
    )

  }
  invisible(projectFolder)
}

write_Rprofile <- function(appendlines,
  path=NULL) {
  # assertthat::assert_that(
  #   !is.null(rstudioapi::getActiveProject()),
  #   msg="This is not a project folder."
  # )
  rprofilepath=".Rprofile"
  if(!is.null(path)) {
    rprofilepath=file.path(
      path, rprofilepath
    )
  }
  .new_lines = appendlines
  if(file.exists(rprofilepath)){
    xfun::read_utf8(rprofilepath) -> .lines
    .lines |>
      paste(collapse="\n") |>
      rlang::parse_exprs() -> expr_lines

    appendlines |>
      paste(collapse="\n") |>
      rlang::parse_exprs() -> exprs_append

    append(expr_lines, exprs_append) -> exprs_all
    exprs_all |>
      duplicated() -> pickDup
    exprs_all[!pickDup] |>
      unlist() -> .x
    .x |>
      purrr::map(
        rlang::expr_deparse) |>
      unlist() |>
      paste(collapse = "\n") -> .new_lines
  }

  xfun::write_utf8(.new_lines, rprofilepath)
}
