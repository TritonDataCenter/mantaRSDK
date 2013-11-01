\name{mantaAttempt}
\alias{mantaAttempt}
\title{REST API Manta Caller with exception handling}
\usage{
  mantaAttempt(action, method, headers, returncode,
    json = TRUE, test = FALSE, verbose = FALSE)
}
\arguments{
  \item{action}{string, optional. Path to a manta object or
  directory. When unspecified, uses current Manta Directory
  and returns JSON listing values for the entire
  directory.}

  \item{method}{string, optional. Default is "GET", passed
  "GET", "POST", "OPTIONS", "PUT", "DELETE" or "HEAD" from
  higher level library callers.}

  \item{headers,}{array of named strings, optional. The
  headers follow the RCurl structure of vector of strings
  where HTTP header tags are the names, values as named
  strings, no semicolons or delimiters.}

  \item{returncode,}{string, optional. Set to expected HTTP
  return code, e.g. 200, 204 - used when test is TRUE}

  \item{json}{logical, optional. Set to FALSE to return R
  data}

  \item{test}{logical, optional, Set to TRUE to return
  logical TRUE/FALSE the request passed or failed. Also
  tests Manta dns resolution.}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication.}
}
\value{
  The Manta reply encoded in JSON or as R data
}
\description{
  mantaAttempt uses the current Manta URL and the current
  working manta directory set with mantaSetwd() and tries
  to list the directory. If test == TRUE, it returns
  pass/fail logical which can stat a directory or object.
  When action is passed a valid path to a Manta object this
  returns the object JSON metadata.
}
\details{
  If passed a Manta subdirectory, it returns the directory
  JSON according to the length limit set with
  mantaSetLimits()

  Note getURL verbose = TRUE writes to stderr - invisible
  on Windows R.
}
\keyword{Manta,}
\keyword{manta}

