\name{mantaAttempt}
\alias{mantaAttempt}
\title{REST API Manta Caller with exception handling}
\usage{
  mantaAttempt(action, json = TRUE, verbose = FALSE)
}
\arguments{
  \item{action}{string, optional. The Manta REST API
  command to transmit. If unspecified, uses current Manta
  Directory and returns JSON listing values.}

  \item{json}{logical, optional. Set to FALSE to return R
  data}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication.}
}
\value{
  The Manta reply encoded in JSON or as R data
}
\description{
  Given path or object name and current Manta directory
  returns URL
}
\keyword{Manta,}
\keyword{manta}

