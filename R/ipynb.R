
# url = "https://github.com/tpemartin/111-1-econDV/blob/main/data3.ipynb"
getRawUrl = function(url) {
  flag_detectBlob = stringr::str_detect(url, "blob")
  if(flag_detectBlob){
    stringr::str_split(url, "/+") |> unlist() -> urlFragments
    indexBlob = stringr::str_which(urlFragments, "blob")
    urlFragments = c(
      "https:/",
      "raw.githubusercontent.com",
      urlFragments[-c(1,2,indexBlob)]
    )
    url = paste0(urlFragments, collapse="/")
  }
  return(url)
}

extract_ipynbCodesFromClipboardUrl = function(){
  url=clipr::read_clip()
  assertthat::assert_that(stringr::str_detect(url, "^https"),
    msg="ipynb document url is not copied to your clipboard yet.")
  extract_ipynbCodes(url)
}
# url = "https://raw.githubusercontent.com/tpemartin/111-1-econDV/main/data3.ipynb"

extract_ipynbCodes = function(url) {
  url=getRawUrl(url)
  filename = paste0(
    url |> stringr::str_extract("[^/]+(?=(\\.ipynb$))"), ".R")
  httr::GET(url) |>
    httr::content() |>
    jsonlite::fromJSON() -> context

  whichIsCode = which(context[['cells']][['cell_type']] == "code")
  codeChunks = context$cells$source[whichIsCode]
  codeChunks |>
    purrr::map(stringr::str_flatten) |>
    unlist() |>
    xfun::write_utf8(con=filename)
  file.edit(filename)
}
