#' Construct colors module
#'
#' @return a list of color toolkits
#' @export
#'
#' @examples
#' cl = Colors()
Colors = function(){
  colors=list()
  colors$hclToHex= hclToHex
  colors$hexToHcl = hexToHcl
  return(colors)
}
#' Convert h,c,l to hex color code
#'
#' @param h
#' @param c
#' @param l
#'
#' @return
#' @export
#'
#' @examples
#' hclToHex(60,40,60) # return #A78D5F
hclToHex = function(h,c,l){
  return(
    colorspace:::hex(
      colorspace::polarLUV(L=l,C=c,H=h)
    )
  )
}

#' Convert hex to h,c,l color code
#'
#' @description This is a wrapper from https://github.com/allopole/datacolor
#' @param hex a character vector of hex codes
#'
#' @return a matrix with nrow = hex length
#' @export
#'
#' @examples
hexToHcl = function(hex){
  datacolor::hex2hcl(hex)
}
