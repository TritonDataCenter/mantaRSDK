\name{mantaInitialize}
\alias{mantaInitialize}
\title{Initialize Manta user variables, check SSH key file exists.}
\usage{
  mantaInitialize(useEnv = TRUE)
}
\arguments{
  \item{useEnv}{logical: \code{TRUE} unless called from
  \code{\link{mantaAccount}} - skips getting env/system
  settings.}
}
\value{
  \code{TRUE} or \code{FALSE} on warn, \code{stop} on
  errors: missing env variables, SSH key.
}
\description{
  Not Exported. Initialization of \code{manta_globals} with
  environment variables checks SSH private key file, sets
  manta cwd to \code{~~/stor}.
}
\keyword{Manta,}
\keyword{manta}

