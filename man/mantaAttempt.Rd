\name{mantaAttempt}
\alias{mantaAttempt}
\title{raw REST API Manta Caller with exception handling for internal
use.}
\usage{
  mantaAttempt(action, method, headers, returncode, limit,
    marker, json = TRUE, test = FALSE, silent = FALSE,
    verbose = FALSE)
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

  \item{limit,}{numeric, optional. Set to limit number of
  returned listed JSON lines - number of directory entries
  Otherwise uses default value in mantaSetLimits}

  \item{marker,}{string, optional. Name or id string of
  directory entry to start next listing of length limit}

  \item{json}{logical, optional. FALSE means return R data,
  TRUE means return JSON data.}

  \item{test}{logical, optional, Set to TRUE to return
  logical TRUE/FALSE as to whether the request passed or
  failed.  Also affects the behavior of the silent
  parameter. See return for output table.}

  \item{silent}{logical, optional. Controls whether 400
  service errors are emitted by cat() or stop() depending
  on the value of test. See return for output table.}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication.}
}
\value{
  The Manta reply data in JSON or R format, OR a logical
  value if test = TRUE. Return values and Manta server
  error message display or stop() behavior depends on
  values of test, silent: test = TRUE, silent = TRUE
  mantaAttempt logical - success returned Errors are logged
  test = TRUE, silent = FALSE mantaAttempt logical -
  success returned Errors are logged, emitted to console.
  test = FALSE, silent = TRUE mantaAttempt data returned
  Errors are logged, empty data on error test = FALSE,
  silent = FALSE mantaAttempt data returned Errors are
  logged, stop() on 400 series errors
}
\description{
  Exported for access HTTP HEAD and metadata retrieval.
}
\details{
  If test == TRUE, it returns pass/fail logical If passed a
  Manta subdirectory, it returns the directory JSON
  according to the length limit set with mantaSetLimits()

  Note getURL verbose = TRUE writes to stderr - invisible
  on Windows R.
}
\keyword{Manta,}
\keyword{manta}

