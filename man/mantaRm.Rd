\name{mantaRm}
\alias{mantaRm}
\title{Removes Manta object specified by full manta path or from current
working manta directory. Option r = TRUE does recursive delete
of object and subdirectories.
Returns TURE if object successfully removed}
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
  Removes Manta object specified by full manta path or from
  current working manta directory. Option r = TRUE does
  recursive delete of object and subdirectories. Returns
  TURE if object successfully removed
}
\keyword{Manta,}
\keyword{manta}

