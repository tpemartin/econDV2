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

  stringr::str_replace_all(origin, coll(replacePattern), replacePattern) -> replacedString
  list(
     replacedString=replacedString,
     reverseReplacePattern=reverseReplacePattern
  )
}

# clipr::read_clip() -> origin
# origin |> protectStringWithSpaces()
