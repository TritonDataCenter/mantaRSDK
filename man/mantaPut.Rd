\name{mantaPut}
\alias{mantaPut}
\title{mantaPut Transfers file, object or buffer to the object specified in mantapath}
\usage{
  mantaPut(filename, mantapath, buffer, object,
    md5 = FALSE, envir, headers, verbose = FALSE)
}
\arguments{
  \item{mantapath}{string, optional. Path to a manta object
  or object name in current working Manta directory. If
  manapath ends in "/" or "\" it is assumed to be
  specifying a Manta subdirectoy and the filename portion
  is appended to it.}

  \item{filename}{optional. Path to local file to PUT. If
  only a filename is given, assumes file is in path
  specified by getwd().}

  \item{buffer}{optional. Raw buffer of data to put.}

  \item{object}{optional. Name of R object to PUT}

  \item{md5}{logical optional. Test md5 hash of data
  before/after PUT transfer}

  \item{envir}{optional. Environment of R object}

  \item{headers}{optional. Headers including R structured
  metadata (up to 4k in user metadata) as array of named
  strings}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication on
  stderr which is invisible on Windows}
}
\value{
  TRUE or FALSE depending on success of PUT transfer
}
\description{
  mantaPut Transfers file, object or buffer to the object
  specified in mantapath
}
\keyword{Manta,}
\keyword{manta}

