\name{mantaRm}
\alias{mantaRm}
\title{Removes specified Manta object, optionally recursive.}
\usage{
  mantaRm(mantapath, r = FALSE, info = TRUE)
}
\arguments{
  \item{mantapath}{string, required.}

  \item{r,}{logical optional. Set TRUE for recursive delete
  of all objects}

  \item{info,}{logical. Show progress information on
  console.}
}
\description{
  Option r = TRUE does recursive delete of object and
  subdirectories. Returns TURE if object and tree
  successfully removed without warnings/errors You can use
  mantaFind to prepare a list of pathnames to objects with
  detailed searching and filtering specifications and then
  use lapply(pathnamelist, mantaRm) to delete the items.
}
\keyword{Manta,}
\keyword{manta}

