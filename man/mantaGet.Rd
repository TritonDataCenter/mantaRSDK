\name{mantaGet}
\alias{mantaGet}
\title{mantaGet Transfers object specified in mantapath to file or buffer}
\usage{
  mantaGet(mantapath, filename, buffer = FALSE,
    metadata = FALSE, verbose = FALSE)
}
\arguments{
  \item{mantapath}{string, optional. Path to a manta object
  or object name in current working Manta directory for
  retrieval.}

  \item{filename}{optional. Assumes this is the filename in
  the current path. Downloads file to the local path
  specified by getwd() if no path specified. If not
  provided downloads to a file with same name as Manta
  object.}

  \item{buffer}{logical required. When TRUE return a buffer
  with data.}

  \item{metadata}{logical optional. Set TRUE to Retrieve R
  metadata in return value.}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication on
  stderr which is invisible on Windows}
}
\value{
  TRUE or FALSE depending on success of GET transfer
}
\description{
  mantaGet Transfers object specified in mantapath to file
  or buffer
}
\keyword{Manta,}
\keyword{manta}

