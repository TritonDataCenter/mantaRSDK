\name{mantaLoad}
\alias{mantaLoad}
\title{Downloads specified Manta object containing R data and uses R function load().}
\usage{
  mantaLoad(mantapath, envir = parent.frame(), info = TRUE,
    verbose = FALSE)
}
\arguments{
  \item{mantapath}{string, optional. Path to a manta R data
  file or file name in current working Manta directory for
  retrieval.}

  \item{envir}{optional. Environment in which to load, See
  load().}

  \item{info}{optional. Print information messages.}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication on
  stderr which is invisible on Windows}
}
\value{
  TRUE or FALSE depending on success of GET transfer
}
\description{
  Downloads specified Manta object containing R data and
  uses R function load().
}
\keyword{Manta,}
\keyword{manta}

