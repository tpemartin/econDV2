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
    eval(substitute(targets::tar_load(target, envir = globalenv()),
                    env = env))
    eval(expr, env=.GlobalEnv)
  }
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
  rstudioapi::savePlotAsImage(file=paste0(target,".png"), format="png", wd[[1]],wd[[2]])
}
