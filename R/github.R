githublink_download_open <- function(){
  link=clipr::read_clip()
  assertthat::assert_that(
    stringr::str_detect(link, "^http"),
    msg=paste(link, " is no a valid url.")
  )
  stringr::str_extract_all(
    link, "(?<=https://github.com/).+(?=/blob)|(?<=blob/)[^#?&]*"
  ) -> linkInfo
  do.call("file.path", list("https://raw.githubusercontent.com", linkInfo[[1]][[1]], linkInfo[[1]][[2]])) -> rawlink

  filename = basename(rawlink)
  if(!dir.exists("temp")) dir.create("temp")
  filepath = file.path("temp", filename)
  xfun::download_file(
    rawlink, mode="wb",
    output= filepath
  )
  file.edit(filepath)
}
