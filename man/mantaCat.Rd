\name{mantaCat}
\alias{mantaCat}
\title{Retrieves object from Manta and uses \code{cat} to print contents to the R console.
\code{mantaCat} is intended for text files, use at your own risk on binary data.}
\usage{
  mantaCat(mantapath, sep = "\n")
}
\arguments{
  \item{mantapath}{vector of character, required.}

  \item{sep}{character, required, separator.}
}
\description{
  Retrieves object from Manta and uses \code{cat} to print
  contents to the R console. \code{mantaCat} is intended
  for text files, use at your own risk on binary data.
}
\examples{
\dontrun{
data <- runif(100)
mantaDump("data")
mantaCat("dumpdata.R")
mantaRm("dumpdata.R")
}
}
\seealso{
  \code{\link{mantaJob.outputs.cat}},
  \code{\link{mantaJob.errors.stderr}}

  Other mantaGet: \code{\link{mantaGet}},
  \code{\link{mantaLoad}}, \code{\link{mantaLoad.ws}}
}
\keyword{Manta}
\keyword{manta}

