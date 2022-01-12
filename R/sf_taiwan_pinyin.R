
# dropboxurl_county = "https://www.dropbox.com/s/qiuvzf0y027azez/mapping_county.json?dl=1"
# dropboxurl_township = "https://www.dropbox.com/s/zsmtshyx0y5e7g0/mapping_township.json?dl=1"
#

translate_map_id_en <- function(dropboxurl, mapping_name) {
  function(sf){
    if(!exists(".map_id", envir=.GlobalEnv)){
      .GlobalEnv[[".map_id"]] = list()
      if(is.null(.GlobalEnv$.map_id[[mapping_name]])){
        .GlobalEnv[[".map_id"]][[mapping_name]] = jsonlite::fromJSON(dropboxurl)
      }
    }
    # browser()
    sf$map_id <- NULL
    mapping_table = .map_id[[mapping_name]]$map_id
    names(mapping_table)=.map_id[[mapping_name]]$map_id_en
    sf$map_id <- mapping_table[sf$map_id_en]

    return(sf)
  }
}
translate_county_map_id <-
  translate_map_id_en(dropboxurl = "https://www.dropbox.com/s/qiuvzf0y027azez/mapping_county.json?dl=1",
    mapping_name="county_mapping")
translate_township_map_id <-
  translate_map_id_en(dropboxurl = "https://www.dropbox.com/s/zsmtshyx0y5e7g0/mapping_township.json?dl=1",
    mapping_name="township_mapping")

# sf_taiwan_simplified |>
#   convert_sf_taiwan_simplified_to_fully_en() -> sf_taiwan_simplified
#
# .sf |> generate_mapping_json()

generate_mapping_json <- function(.sf){

  mapping_county <- .sf$county_level[c("map_id", "map_id_en")]
  mapping_county$geometry <- NULL
  mapping_township <- .sf$township_level[c("map_id", "map_id_en")]
  mapping_township$geometry <- NULL

  mapping_county |>
    jsonlite::toJSON() |>
    xfun::write_utf8("/Users/martinl/Dropbox/github-data/mapping_county.json")
  mapping_township |>
    jsonlite::toJSON() |>
    xfun::write_utf8("/Users/martinl/Dropbox/github-data/mapping_township.json")

}

# helpers -----------------------------------------------------------------
convert_sf_taiwan_simplified_to_fully_en <- function(sf_taiwan_simplified){
  sf_taiwan_simplified -> .sf

  .sf$縣市 |> add_map_id_en() -> .sf$縣市
  .sf$鄉鎮區 |> add_map_id_en() -> .sf$鄉鎮區
  .sf$`台灣本島`$縣市 |> add_map_id_en() -> .sf$`台灣本島`$縣市
  .sf$`台灣本島`$鄉鎮區 |> add_map_id_en() -> .sf$`台灣本島`$鄉鎮區

  names(.sf) <- c("county_level", "township_level", "main_island")
  names(.sf$main_island) <- c("county_level", "township_level")

  return(.sf)
}

add_map_id_en <- function(.sf){
  .sf |>
    dplyr::mutate(
      map_id_en=get_pinyin(map_id)
    )
}
get_pinyin <- function(chr){
  chr |>
    pinyin::py(dic=pinyin::pydic(method="toneless"))
}
