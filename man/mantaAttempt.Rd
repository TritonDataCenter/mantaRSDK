\name{mantaAttempt}
\alias{mantaAttempt}
\title{raw REST API Manta Caller with exception handling for internal
use.}
\usage{
mantaAttempt(action, method, headers, returncode, limit, marker, json = TRUE,
  test = FALSE, silent = FALSE, verbose = FALSE)
}
\arguments{
  \item{action}{character, optional. Path to a manta object
  or directory with optional query terms. When unspecified,
  uses current Manta Directory and returns JSON listing
  values for the directory.}

  \item{method}{character, optional. Default is
  \code{"GET"}, passed \code{"GET", "POST", "OPTIONS",
  "PUT", "DELETE"} or \code{"HEAD"} from higher level
  library callers.}

  \item{headers,}{array of named characters, optional. The
  headers follow the \code{RCurl} structure of vector of
  characters where HTTP header tags are the names, values
  as named characters, no semicolons or delimiters.}

  \item{returncode,}{character, optional. Set to expected
  HTTP return code, e.g. \code{"200", "204"} - used when
  test is TRUE.}

  \item{limit,}{numeric, optional. Set to limit number of
  returned listed JSON lines - number of directory entries
  Otherwise uses default value in \code{mantaSetLimits}}

  \item{marker,}{character, optional. Name or id character
  value of directory entry to start next listing segment of
  length \code{limit}.}

  \item{json}{logical, optional. \code{FALSE} means return
  R data, \code{TRUE} means return JSON data.}

  \item{test}{logical, optional, Set to \code{TRUE} to
  return logical as to whether the request passed or
  failed.  Also affects the behavior of the \code{silent}
  parameter. See Value for output table.}

  \item{silent}{logical, optional. Controls whether \code{>
  "400"} service errors are emitted by \code{cat} or
  \code{stop} depending on the value of test. See Value for
  output table.}

  \item{verbose}{logical, optional. Passed to \code{RCurl}
  \code{GetURL}, Set to \code{TRUE} to see background HTTPS
  REST communication.}
}
\value{
The Manta reply data in JSON or R format, OR a logical
value if \code{test = TRUE}. Return values and Manta server
error message display or \code{stop} behavior depends on
values of \code{test, silent}:\cr \cr \code{test = TRUE,
silent = TRUE}\cr logical - success returned, Errors are
logged.\cr \code{test = TRUE, silent = FALSE}\cr logical -
success returned, Errors are logged, emitted to console.\cr
\code{test = FALSE, silent = TRUE}\cr data returned, Errors
are logged, empty data on error.\cr \code{test = FALSE,
silent = FALSE} data returned, Errors are logged, stop() on
400 series errors.\cr
}
\description{
Exported to access HTTPS Manta API and metadata retrieval.
}
\details{
If \code{test == TRUE}, it returns pass/fail logical If
passed a Manta subdirectory, it returns the directory JSON
according to the length limit set with
\code{mantaSetLimits}

Note getURL \code{verbose = TRUE} writes to UNIX
\code{stderr} which is invisible on Windows R.
}
\keyword{Manta}

