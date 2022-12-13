makePlotAtCursor = function(){
  if(!require(targets)){
    install.packages("targets")}
  if(!require(rstudioapi)){
    install.packages("rstudioapi")}
  context <- rstudioapi::getActiveDocumentContext()
  target <- targets:::rstudio_symbol_at_cursor(context)
  if (!is.null(target)) {
    targets:::cli_blue_bullet(paste("Loading target", target, "into global environment."))
    targetSymbol = as.symbol(target)
    expr = rlang::expr(print((!!targetSymbol)$make()))
    env <- list(target = targetSymbol)
    tryCatch(
      {
        eval(substitute(targets::tar_load(target, envir = globalenv()),
                        env = .GlobalEnv))
      },
      error=function(e){
        warning('There is an error. ')
      }
    )

    eval(expr, env=.GlobalEnv)
  }
  clipr::write_clip(
    paste0(target,"$make()")
  )
  invisible(target)
}
savePlotAtCursor = function(){
  target = makePlotAtCursor()
  wd <- rstudioapi::showPrompt(
    title="Size",
    message = c("Set your width x height, separting two numbers by x:"),
    default = c("800 x 800")
  )
  wd |> stringr::str_extract_all("[0-9]+") |> unlist() |>
    as.numeric() -> wd

  fm <- rstudioapi::showPrompt(
    title="Format",
    message = c('Set graph format "png", "jpeg", "bmp", "tiff", "emf", "svg", "eps"'),
    default = c("png")
  )
  filename = paste0(target,".",fm)
  if(file.exists(filename)) file.remove(filename)
  rstudioapi::savePlotAsImage(file=filename, format=fm, wd[[1]],wd[[2]])
  clipr::write_clip(
    paste0(target,"$make()")
  )
}
getCursorTarget = function(){
  if(!require(targets)){
    install.packages("targets")}
  if(!require(rstudioapi)){
    install.packages("rstudioapi")}
  context <- rstudioapi::getActiveDocumentContext()
  target <- targets:::rstudio_symbol_at_cursor(context)
  return(target)
}
