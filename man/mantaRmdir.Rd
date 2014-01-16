\name{mantaRmdir}
\alias{mantaRmdir}
\title{Removes Manta subdirectory.}
\usage{
  mantaRmdir(mantapath, info = TRUE)
}
\arguments{
  \item{mantapath}{character, required. Not vectorized.}

  \item{info}{logical. Set FALSE to suppress console
  messages.}
}
\value{
  \code{TRUE} or \code{FALSE} depending on success of
  remove.
}
\description{
  Removes specified Manta subdirectory. Non-recursive, not
  vectorized.
}
\details{
  Removes directory. Specify absolute (e.g.
  \code{~~/stor/myobject.txt} ) or relative path from the
  current working Manta directory.
}
\examples{
\dontrun{
## Save current working Manta directory
mantaGetwd() -> tempdir
## Relative mantapath use:
mantaSetwd.stor()
mantaMkdir("a_test")
mantaLs.l()
mantaRmdir("a_test")
mantaLs.l()
## Absolute mantapath use:
mantaMkdir("~~/public/b_test")
mantaLs.l("~~/public")
mantaRmdir("~~/public/b_test")
mantaLs.l("~~/public")
## Restore current working Manta directory
mantaSetwd(tempdir)
}
}
\seealso{
  Other Directory: \code{\link{mantaGetwd}},
  \code{\link{mantaMkdir}}, \code{\link{mantaRm}},
  \code{\link{mantaSetwd}},
  \code{\link{mantaSetwd.public}},
  \code{\link{mantaSetwd.reports}},
  \code{\link{mantaSetwd.stor}}, \code{\link{mantaSnapln}}
}
\keyword{Manta,}
\keyword{manta}

