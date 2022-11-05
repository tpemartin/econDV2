#' Special function for targets::tar_target(a, fun()) usage
#'
#' @param x a symbol
#' @param ... other tar_target arguments
#'
#' @return tar_target call result
#' @export
#'
#' @examples \donotrun{
#' tar_target(a, fun()) # the same as
#' a %t=% fun()
#' }
`%t=%` = function(x,...){
  exprX = rlang::enexpr(x)
  exprsY <<- rlang::enexprs(...)
  enquosY <<- rlang::enquos(...)
  unquoExprs = rlang::expr(targets::tar_target(!!exprX, !!!enquosY))
  rlang::eval_tidy(unquoExprs)
}
