generateIsoCodeData = function(){
  jsonlite::fromJSON(
    "https://gist.githubusercontent.com/jacobbubu/060d84c2bdf005d412db/raw/845c78f55e49fee89814bdc599355069f07b7ee6/countries.json"
  ) -> isocodes
  return(isocodes)
}

