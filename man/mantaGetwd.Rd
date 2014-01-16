\name{mantaGetwd}
\alias{mantaGetwd}
\title{Gets current working directory on Manta.}
\usage{
  mantaGetwd()
}
\description{
  The current working directory is stored internally in
  \code{mantaRSDK} on your local system and is not saved.
  between sessions. It initializes to the root directory of
  private Manta storage: \code{~~/stor}.
}
\examples{
\dontrun{
## Manta working directory
mantaGetwd()
mantaGetwd() -> tempdir
mantaSetwd.public()
mantaLs.l()
mantaSetwd(tempdir)
}
}
\seealso{
  Other Directory: \code{\link{mantaMkdir}},
  \code{\link{mantaRm}}, \code{\link{mantaRmdir}},
  \code{\link{mantaSetwd}},
  \code{\link{mantaSetwd.public}},
  \code{\link{mantaSetwd.reports}},
  \code{\link{mantaSetwd.stor}}, \code{\link{mantaSnapln}}
}
\keyword{Manta,}
\keyword{manta}

