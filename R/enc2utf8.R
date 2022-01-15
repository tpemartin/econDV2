enc2utf8_all <- function(.sf){
  for(.x in names(.sf)){
    if(any(class(.sf[[.x]]) == "sfc")) next
    enc2utf8(.sf[[.x]]) -> .sf[[.x]]
  }
  return(.sf)
}
get_sf_taiwan4windows <- function(){
  econDV2::sf_taiwan_simplified -> sf_taiwan_simplified
  enc2utf8(names(sf_taiwan_simplified)) -> names(sf_taiwan_simplified)
  sf_taiwan_simplified[c(1,2)] |>
    purrr::map(enc2utf8_all) -> sf_taiwan_simplified[c(1,2)]

  sf_taiwan_simplified[[3]] |>
    purrr::map(enc2utf8_all) -> sf_taiwan_simplified[[3]]
  return(sf_taiwan_simplified)
}
# sf_taiwan_simplified <- get_sf_taiwan4windows()
