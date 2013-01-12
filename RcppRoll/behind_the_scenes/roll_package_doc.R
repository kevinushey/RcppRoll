write <- function(..., conn=stdout() ) {
  cat( paste0(..., "\n"), file=conn )
}

file <- file( "./R/package_description.R", open="w" )

exported_functions <- grep( "roll_", list.files("./R"), value=TRUE )
exported_functions <- gsub("\\.R", "", exported_functions)
exported_functions_rox <- strsplit(
  paste( "\\itemize{\n", 
         paste( sep="", collapse="\n",
         "\\item{\\code{\\link{", exported_functions, "}}}"
                ),
         "\n}" ), "\n" )[[1]]


write( conn=file,
       "#' RcppRoll\n",
       "#' \n",
       "#' This package implements a number of 'roll'-ing functions for \\R \n",
       "#' vectors and matrices.\n",
       "#' \n",
       "#' Currently, the exported functions are:\n",
       paste("#' ", exported_functions_rox, "\n", collapse="" ),
       "#' \n",
       "#' @name RcppRoll\n",
       "#' @docType package\n",
       "#' @useDynLib RcppRoll\n",
       "#' @seealso \\code{\\link{rollit}} for 'roll'-ing your own custom functions.\n",
       "NULL\n"
)

close(file)