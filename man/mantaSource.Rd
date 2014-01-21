\name{mantaSource}
\alias{mantaSource}
\title{Downloads specified Manta R source code file and applies \code{source} to parse/load it.}
\usage{
mantaSource(mantapath, local = FALSE, verbose = FALSE,
  max.deparse.length = 150, encoding = getOption("encoding"),
  keep.source = getOption("keep.source"))
}
\arguments{
  \item{mantapath}{character, optional. Path to a manta R
  code file or file name in current working Manta directory
  for retrieval. Not vectorized.}

  \item{local}{logical optional. See \code{source}.}

  \item{verbose}{logical, optional. Passed to \code{RCurl}
  \code{GetUR}L, Set to \code{TRUE} to see background REST
  communication on \code{stderr}. Note this is not visible
  on Windows.}

  \item{max.deparse.length}{optional. See \code{source}.}

  \item{encoding}{optional. See \code{source}.}

  \item{keep.source}{optional. See \code{source}.}
}
\value{
\code{TRUE} or \code{FALSE} depending on success of
download.
}
\description{
Downloads specified Manta R source code file and applies
\code{source} to parse/load it.
}
\examples{
\dontrun{
data <- runif(100)
ls()
mantaDump("data")
rm(data)
mantaCat("dumpdata.R")
ls()
mantaSource("dumpdata.R")
ls()
mantaRm("dumpdata.R")
rm(data)
}
}
\seealso{
\code{\link{mantaDump}}

Other mantaGet: \code{\link{mantaCat}};
\code{\link{mantaGet}}; \code{\link{mantaLoad.ws}};
\code{\link{mantaLoad}}; \code{\link{mantaSave}}
}
\keyword{Manta}

