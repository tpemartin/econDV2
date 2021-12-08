#' Initiate an object with self saving method.
#'
#' @param objectname A symbol or a character
#'
#' @return
#' @export
#'
#' @examples Object(world)
Object <- function(objectname){
  chr_objname <- expr_objname <- rlang::enexpr(objectname)
  # browser()
  chr_objname <- rlang::expr_deparse(expr_objname)
  flag_chr <- stringr::str_detect(chr_objname,"\"")
  if(flag_chr) {
    chr_objname |> stringr::str_remove_all("\"") -> chr_objname
    expr_objname <- rlang::parse_expr(chr_objname)
  }

  rlang::expr(
    !!expr_objname <- list()
  ) -> declare_obj
  eval(declare_obj, envir = .GlobalEnv)
  list(
    save=function(){
      filename = file.path("data",paste0(chr_objname,".Rds"))
      if(!dir.exists("data")) dir.create("data")
      saveRDS(.GlobalEnv[[chr_objname]], filename)
      message(filename," is saved.")
    }
  ) -> value2assign
  assign(chr_objname, value2assign, envir = .GlobalEnv)
}
