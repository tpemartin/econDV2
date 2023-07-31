#' Identify county and township based on lon lat
#'
#' @param df a data frame with numeric class lon and lat columns
#'
#' @return a data frame augmented with Taiwan's county and township
#' @export
augment_county_township_using_lon_lat <- function(df) {
  assertthat::assert_that(
    all(c("lat","lon") %in% names(df)),
    msg = "Make sure lat and lon columns are in the data frame, and are of numeric class."
  )

  mp <- econDV2::Map()
  mp$sf$get_sf_taiwan_simplified() -> list_tw_sf
  list_tw_sf$縣市 -> sf_county
  list_tw_sf$鄉鎮區 -> sf_township
  list_tw_sf$離島 -> list_sf_islands

  sf_township |>
    split(sf_township$is_in.city) -> list_township_sf

  df <- augment_geometry_points_from_lon_lat(df, setCRS_based_on = sf_county)

  # dataEnv = new.env()
  # dataEnv$df = df
  # dataEnv$df$county = ""
  # dataEnv$df$township = ""
  df$county= ""
  df$township=""

  df <- augment_county_to_sf_dataframe(df, sf_county)


  # 各二級行政區包含誰
  # split township simple feature based on county
  split_sf_township = sf_township |>
    split(
      sf_township$is_in.city
    )

  # split data based on county
  split_df <- split(
    df,
    df$county
  )

  # for each township obtain indices of row in df that belong to it
  indexOf_withinEachCounty_eachTownshipContains_result <-
    obtain_rowIndex_obs_belong_to_each_township(split_df, split_sf_township)

  split_df <- augment_township_to_split_df(split_df, split_sf_township, indexOf_withinEachCounty_eachTownshipContains_result) |>
    identify_penhu_township(list_sf_islands) |>
    identify_kingmen_township(list_sf_islands)

  df_augmented <- unsplit_df(split_df)

}
# helpers -----
augment_geometry_points_from_lon_lat <- function(df, setCRS_based_on) {
  seq_along(df$lat) |>
    purrr::map(
      ~sf::st_point(c(df$lon[[.x]], df$lat[[.x]]))
    ) -> list_points

  sf::st_sfc(list_points) -> sfc_shopLocations
  sfc_shopLocations <- sf::st_set_crs(sfc_shopLocations, sf::st_crs(setCRS_based_on))
  sf::st_geometry(df) <- sfc_shopLocations
  df
}
augment_county_to_sf_dataframe <- function(df, sf_county) {
  assertthat::assert_that(
    "sf" %in% class(df),
    msg="data frame needs to be a simple feature data frame"
  )
  dataEnv = new.env()
  dataEnv$df = df
  dataEnv$df$county = ""

  # 各一級行政區包含誰
  sf_county |>
    sf::st_contains(
      df
    ) -> countiesInclude

  purrr::walk(
    seq_along(countiesInclude),
    ~{
      targetCounty = sf_county[.x,]$name
      # dataEnv$df[countiesInclude[[.x]],]$county = targetCounty
      dataEnv$df$county[countiesInclude[[.x]]] = targetCounty
    }
  )
  return(dataEnv$df)
}
obtain_rowIndex_obs_belong_to_each_township <- function(split_df, split_sf_township) {
  purrr::map(
    seq_along(split_sf_township),
    purrr::safely(function(.x){
      targetCounty = names(split_sf_township[.x])
      targetTownship_sf = split_sf_township[[.x]]
      targetDF = split_df[[targetCounty]]
      targetTownship_sf |>
        sf::st_contains(
          targetDF
        )
    })
  ) |>
    setNames(names(split_sf_township)) -> indexOf_withinEachCounty_eachTownshipContains

  indexOf_withinEachCounty_eachTownshipContains |>
    purrr::map(
      ~{purrr::pluck(.x,"result")}
    ) -> indexOf_withinEachCounty_eachTownshipContains_result
  indexOf_withinEachCounty_eachTownshipContains_result
}
identify_penhu_township <- function(split_df, list_sf_islands) {
  list_sf_islands$澎湖 |>
    sf::st_contains(
      split_df[[1]]
    ) -> indexOf_penhu_eachTownshipContains_result

  for(.x in seq_along(indexOf_penhu_eachTownshipContains_result)){
    townshipX <- list_sf_islands$澎湖$name[[.x]]
    whichBelongToTheTownshipX <-
      indexOf_penhu_eachTownshipContains_result[[.x]]
    if(length(whichBelongToTheTownshipX) ==0) next
    split_df[[1]]$county[whichBelongToTheTownshipX] <- "澎湖縣"
    split_df[[1]]$township[whichBelongToTheTownshipX] <- townshipX
  }
  return(split_df)
}
identify_kingmen_township <- function(split_df, list_sf_islands) {
  list_sf_islands$金門 |>
    sf::st_contains(
      split_df[[1]]
    ) -> indexOf_penhu_eachTownshipContains_result

  for(.x in seq_along(indexOf_penhu_eachTownshipContains_result)){
    townshipX <- list_sf_islands$金門$name[[.x]]
    whichBelongToTheTownshipX <-
      indexOf_penhu_eachTownshipContains_result[[.x]]
    if(length(whichBelongToTheTownshipX) ==0) next
    split_df[[1]]$county[whichBelongToTheTownshipX] <- "金門縣"
    split_df[[1]]$township[whichBelongToTheTownshipX] <- townshipX
  }
  return(split_df)
}

augment_township_to_split_df <- function(split_df, split_sf_township, indexOf_withinEachCounty_eachTownshipContains_result) {
  envSplit = new.env()
  envSplit$split_df = split_df
  countyNames = names(indexOf_withinEachCounty_eachTownshipContains_result)
  seq_along(indexOf_withinEachCounty_eachTownshipContains_result) |>
    purrr::map(
      ~{
        # .x=1
        countyNameX = countyNames[[.x]]
        target_indexOf_withinEachCounty_eachTownshipContains_result <- indexOf_withinEachCounty_eachTownshipContains_result[[countyNameX]]
        target_indexOf_withinEachCounty_eachTownshipContains_result |>
          purrr::map_int(length) -> lengthOfIndices
        whichTownshipHasData <- which(lengthOfIndices!=0)

        whichTownshipHasData |>
          purrr::walk(
            ~{
              # .x=3
              townshipX = split_sf_township[[countyNameX]][.x,]$name
              whichBelongsToTownshipX <- target_indexOf_withinEachCounty_eachTownshipContains_result[[.x]]
              envSplit$split_df[[countyNameX]][whichBelongsToTownshipX,]$township <- townshipX
            }
          )
      }
    )
  envSplit$split_df
}

unsplit_df <- function(split_df) {
  split_df |>
    purrr::map_dfr(
      ~{
        sf::st_drop_geometry(.x) }
    ) -> df_augmented
  df_augmented
}

