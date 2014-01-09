\name{mantaGet}
\alias{mantaGet}
\title{Downloads Manta object(s) (vectorized) specified to file(s) or buffer.}
\usage{
  mantaGet(mantapath, filename, buffer = FALSE,
    metadata = FALSE, info = TRUE, verbose = FALSE)
}
\arguments{
  \item{mantapath}{vector of character, optional. Path to a
  manta object or object name in current working Manta
  directory for retrieval. Vectorized, e.g.
  mantaGet(mantaLs.paths(items = 'o')) will download the
  contents of the Manta working directory to your local
  working R directory, and mantaGet(mantaLs.paths(items =
  'o'), metadata = TRUE) will download and return just the
  metadata in R format for the Manta working directory
  contents.}

  \item{filename}{optional. Assumes this is the filename in
  the current path. Downloads file to the local path
  specified by getwd() if no path specified. If not
  provided downloads to a file with same name as Manta
  object.}

  \item{buffer}{logical required. When TRUE return a buffer
  with data. Not supported for vectorized mantapath input.}

  \item{metadata}{logical optional. Set TRUE to Retrieve R
  metadata in return value.}

  \item{info}{logical. Set to FALSE to suppress Downloading
  message.}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication on
  stderr which is invisible on Windows}
}
\value{
  TRUE or FALSE depending on success of GET transfer
}
\description{
  One limitation of the mantaRSDK is that it is not
  designed to handle large (multi-Gigabyte or larger)
  objects. Uploads - mantaPut() - work from files, but
  Downloads - mantaGet() - fill the R memory space to their
  completion before being written to a file. To download
  files larger than your R memory, use the Manta Node.js
  command line tool mget. The Node.js based Manta command
  line tools employ streams, so object size is not a
  limitation.
}
\keyword{Manta,}
\keyword{manta}

