\name{mantaAttempt}
\alias{mantaAttempt}
\title{REST API Manta Caller with exception handling}
\usage{
  mantaAttempt(action, json = TRUE, test = FALSE,
    verbose = FALSE)
}
\arguments{
  \item{action}{string, optional. Path to a manta object or
  directory. When unspecified, uses current Manta Directory
  and returns JSON listing values for the entire
  directory.}

  \item{json}{logical, optional. Set to FALSE to return R
  data}

  \item{test}{logical, optional, Set to TRUE to return
  logical TRUE/FALSE the object/directory exists or does
  not. Also tests Manta name resolution.}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication.}
}
\value{
  The Manta reply encoded in JSON or as R data
}
\description{
  Note getURL verbose = TRUE writes to stderr - invisible
  on Windows R. mantaAttempt uses the current Manta URL and
  the current working manta directory set with mantaSetwd()
  and tries to list the directory. If test == TRUE, it
  returns pass/fail logical otherwise it behaves like stat.
  When action is passed a valid path to a Manta object this
  returns the object JSON metadata.
}
\details{
  If passed a Manta subdirectory, it returns the directory
  JSON according to the length limit set with
  mantaSetLimits()
}
\keyword{Manta,}
\keyword{manta}

