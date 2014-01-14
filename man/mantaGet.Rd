\name{mantaGet}
\alias{mantaGet}
\title{Downloads specified Manta object(s), to file(s) or buffer.}
\usage{
  mantaGet(mantapath, filename, buffer = FALSE,
    metadata = FALSE, info = TRUE, verbose = FALSE)
}
\arguments{
  \item{mantapath}{vector of character, optional. Path to a
  manta object or object name in current working Manta
  directory for retrieval. Vectorized,}

  \item{filename}{optional. Assumes this is the target
  filename in the current path. Downloads file to the local
  path specified by \code{getwd} if full path not
  specified. If \code{filename} is absent, downloads to a
  file with same name as Manta object.}

  \item{buffer}{logical required. When \code{TRUE} return a
  buffer with data. Not supported for vectorized
  \code{mantapath} input.}

  \item{metadata}{logical optional. Set \code{TRUE} to
  retrieve R metadata.}

  \item{info}{logical. Set \code{FALSE} to suppress
  Downloading console messages.}

  \item{verbose}{logical, optional. Passed to \code{RCurl}
  \code{GetURL}, Set to \code{TRUE} to see background HTTPS
  REST communication on \code{stderr}. Note this is not
  visible on Windows.}
}
\value{
  \code{TRUE} or \code{FALSE} depending on success of GET
  transfer
}
\description{
  One limitation of the \code{mantaRSDK} is that it is not
  designed to handle large (multi-Gigabyte or larger)
  objects. Uploads - \code{mantaPut} - work from files, but
  Downloads - \code{mantaGet} - fill the R memory space to
  their completion before being written to a file. To
  download files larger than your R memory, use the Manta
  Node.js command line tool \code{mget}. The Node.js based
  Manta command line tools employ streams, so object size
  is not a limitation.
}
\examples{
\dontrun{
data <- runif(100)
mantaDump("data")
rm(data)
mantaGet("dumpdata.R")
mantaRm("dumpdata.R")
source("dumpdata.R")
ls()

mantaGet(mantaLs.paths(items = 'o'))
## Downloads the objects in your Manta working directory to your local working R directory with
## the same filenames.

mantaGet(mantaLs.paths(items = 'o'), metadata = TRUE)
## Downloads and return just the metadata in R format for the Manta working directory contents.
}
}
\seealso{
  \code{\link{mantaPut}}

  Other mantaGet: \code{\link{mantaCat}},
  \code{\link{mantaLoad}}, \code{\link{mantaLoad.ws}}
}
\keyword{Manta,}
\keyword{manta}

