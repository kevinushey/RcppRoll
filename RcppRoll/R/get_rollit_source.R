#' Get and Edit Source File Associated with User-Generated 'rollit' Function
#' 
#' This function returns and opens the source file associated with
#' a particular 'rollit' function for debugging. We use \R's
#' \code{file.edit} interface.
#' 
#' @param fun The generated 'rollit' function.
#' @param edit boolean; open the C++ source file in a text editor?
#' @param RStudio boolean; open the C++ source file in RStudio?
#' @param ... Optional arguments passed to \code{\link{file.edit}}.
#' @export
get_rollit_source <- function(fun, edit=TRUE, RStudio=FALSE, ...) {
  
  file <- get( "outFile", envir=environment( fun ) )
  if( !file.exists(file) ) {
    stop("File does not exist!")
  }
  
  if( edit ) {
    if( RStudio ) {
      file.edit( file, editor="RStudio", ... )
    } else {
      file.edit( file, ... )
    }
  }
  return( file )
  
}
