# labs( title='The Industry of All Non-U.S. Issuers',
getStringsNeedProtection <- function(string) {
  pattern_strProtect = "(?<=\")[^\"]*(?=\")|(?<=')[^\"]*(?=')|(?<=`)[^`]+(?=`)"
  stringr::str_extract_all(
     string,
     pattern_strProtect
  ) -> strNeedsProtection
  # remove c("a", "b") case which ", " is picked wrongly
  stringr::str_subset(unlist(strNeedsProtection), "^\\s*,\\s*$", negate=T) ->
    strNeedsProtection
  stringr::str_which(
    string,
    pattern_strProtect
  ) -> strWhichNeedsProtection
  strNeedsProtection |> unlist() |> unique() -> stringNeedsProtection
  return(list(pattern=stringNeedsProtection, index=strWhichNeedsProtection))
}

protectStringWithSpaces <- function(origin) {
  origin |>
      getStringsNeedProtection() -> Protect

  Protect$pattern -> stringNeedsProtection
  Protect$index -> whereInStringNeedsProtection
  protection_list <- list()
  if(length(stringNeedsProtection)!=0){
     protectors <- paste0("..",LETTERS,"..", sep="")
   if(length(stringNeedsProtection) > length(LETTERS)){
      rounds = ceiling(length(stringNeedsProtection)/length(LETTERS))
      protectors <-
         paste("..", rep(LETTERS, rounds), LETTERS[1:rounds],"..", sep="")
   }
  replacePattern = setNames(
     protectors[1:length(stringNeedsProtection)], stringNeedsProtection
  )
  reverseReplacePattern = setNames(
     stringNeedsProtection, protectors[1:length(stringNeedsProtection)]
  )

  stringr::str_replace_all(origin[whereInStringNeedsProtection], stringr::coll(replacePattern), replacePattern) -> origin[whereInStringNeedsProtection]
  origin -> replacedString
  protection_list <- list(
     replacedString=replacedString,
     reverseReplacePattern=reverseReplacePattern
  )
  }
  return(protection_list)
}

# clipr::read_clip() -> origin
# origin |> protectStringWithSpaces()
