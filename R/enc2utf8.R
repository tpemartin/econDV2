enc2utf8_all <- function(.sf){
  for(.x in names(.sf)){
    if(any(class(.sf[[.x]]) == "sfc")) next
    enc2utf8(.sf[[.x]]) -> .sf[[.x]]
  }
  .sf |> sf::st_as_sf() -> .sf
  return(.sf)
}
get_sf_taiwan4windows <- function(){
  econDV2::sf_taiwan_simplified -> sf_taiwan_simplified
  names(sf_taiwan_simplified) <-
    c("縣市", "鄉鎮區", "台灣本島")
  sf_taiwan_simplified[c(1,2)] |>
    purrr::map(enc2utf8_all) -> sf_taiwan_simplified[c(1,2)]

  sf_taiwan_simplified[[3]] |>
    purrr::map(enc2utf8_all) -> sf_taiwan_simplified[[3]]

  # sf_taiwan_simplified[[1]] |>
  #   sf::st_as_sf() -> sf_taiwan_simplified[[1]]
  # sf_taiwan_simplified[[2]] |>
  #   sf::st_as_sf() -> sf_taiwan_simplified[[2]]
  # sf_taiwan_simplified[[3]][[1]] |>
  #   sf::st_as_sf() -> sf_taiwan_simplified[[3]][[1]]
  # sf_taiwan_simplified[[3]][[2]] |>
  #   sf::st_as_sf() -> sf_taiwan_simplified[[3]][[2]]
  return(sf_taiwan_simplified)
}
# sf_taiwan_simplified <- get_sf_taiwan4windows()
