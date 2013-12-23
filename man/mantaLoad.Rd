\name{mantaLoad}
\alias{mantaLoad}
\title{mantaLoad Loads R data object specified in mantapath.}
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
  mantaLoad Loads R data object specified in mantapath.
}
\keyword{Manta,}
\keyword{manta}

