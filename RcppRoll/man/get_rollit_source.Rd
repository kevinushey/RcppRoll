\name{get_rollit_source}
\alias{get_rollit_source}
\title{Get and Edit Source File Associated with 'rollit' Function}
\usage{
  get_rollit_source(fun, RStudio = FALSE, edit = TRUE, ...)
}
\arguments{
  \item{fun}{The generated 'rollit' function.}

  \item{edit}{boolean; open the C++ source file in a text
  editor?}

  \item{RStudio}{boolean; open the C++ source file in
  RStudio?}

  \item{...}{Optional arguments passed to
  \code{\link{file.edit}}.}
}
\description{
  This function returns and opens the source file
  associated with a particular 'rollit' function for
  debugging. We use \R's \code{file.edit} interface.
}

