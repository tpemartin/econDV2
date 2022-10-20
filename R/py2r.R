convertActivePy2R=function(){
  rstudioapi::getActiveDocumentContext() -> actfile
  convertColabPy2R(actfile$path)
}

readpy = function(filename="./R/week_6.py")
{
  filename |> xfun::read_utf8()

}

convertColabPy2R = function(filename="./R/week_6.py")
{
  assertthat::assert_that(
    stringr::str_detect(filename, "\\.py$"),
    msg="the active file is not a py file."
  )
  readpy(filename) -> pylines
  which(pylines == '"""') -> cutpointsEnding
  pylines |>
    stringr::str_which("^\"\"\"") -> cutpoints
  pylines |>
    stringr::str_which("\"\"\"$") -> onelineCutPoints
  c(cutpointsEnding, setdiff(cutpoints,onelineCutPoints)) |>
    sort() -> cutpoints
  startCutpoints = seq(1,length(cutpoints),by=2)
  cutpoints[startCutpoints] = cutpoints[startCutpoints] -1
  indices = 1:length(pylines)
  indices |>
    cut(
      c(cutpoints, Inf)
    ) -> fct_indices
  levels(fct_indices) ->   levels_indices
  levels_indices[seq(1,length(levels_indices), by=2)] -> commentInterval

  lines2comment <- which(fct_indices %in% commentInterval)
  lines2comment
  pylines[lines2comment] <-
    paste("# ", pylines[lines2comment])

  pylines |>
    stringr::str_which("\"\"\"$") -> onelineCutPoints


  pylines[onelineCutPoints] <-
    paste("# ", pylines[onelineCutPoints])

  outfilename = stringr::str_replace(filename, "\\.py$", "\\.R")
  pylines |> xfun::write_utf8(con=outfilename)
  file.edit(outfilename)
}

