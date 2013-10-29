\name{mantaAttempt}
\alias{mantaAttempt}
\title{REST API Manta Caller with exception handling}
\usage{
  mantaAttempt(action, json = TRUE, test = FALSE,
    verbose = FALSE)
}
\arguments{
  \item{action}{string, optional. The Manta REST API
  command to transmit. If unspecified, uses current Manta
  Directory and returns JSON listing values for first 256
  objects.}

  \item{json}{logical, optional. Set to FALSE to return R
  data}

  \item{test}{logical, optional, Set to TRUE to return
  logical pass/fail}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication.}
}
\value{
  The Manta reply encoded in JSON or as R data
}
\description{
  Note getURL verbose = TRUE writes to stderr - invisible
  on Windows R.
}
\keyword{Manta,}
\keyword{manta}

