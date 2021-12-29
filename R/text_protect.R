# labs( title='The Industry of All Non-U.S. Issuers',
getStringsNeedProtection <- function(string) {
  pattern_strProtect = "(?<=\")[^\"]*(?=\")|(?<=')[^\"]*(?=')"
  stringr::str_extract_all(
     string,
     pattern_strProtect
  ) -> strNeedsProtection
  strNeedsProtection |> unlist() |> unique()
}

protectStringWithSpaces <- function(origin) {
  origin |>
      getStringsNeedProtection() -> stringNeedsProtection
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

  stringr::str_replace_all(origin, stringr::coll(replacePattern), replacePattern) -> replacedString
  protection_list <- list(
     replacedString=replacedString,
     reverseReplacePattern=reverseReplacePattern
  )
  }
  return(protection_list)
}

# clipr::read_clip() -> origin
# origin |> protectStringWithSpaces()
