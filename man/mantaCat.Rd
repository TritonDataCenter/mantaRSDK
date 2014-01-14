\name{mantaCat}
\alias{mantaCat}
\title{Retrieves object from Manta and uses \code{cat} to print contents to the R console.}
\usage{
  mantaCat(mantapath, sep = "\n")
}
\arguments{
  \item{mantapath}{vector of character, required.}

  \item{sep}{character, required, separator.}
}
\description{
  Retrieves object from Manta and uses \code{cat} to print
  contents to the R console.
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

  Other mantaGet: \code{\link{mantaGet}}
}
\keyword{Manta,}
\keyword{\code{mantaCat}}
\keyword{at}
\keyword{binary}
\keyword{data.}
\keyword{files,}
\keyword{for}
\keyword{intended}
\keyword{is}
\keyword{manta}
\keyword{on}
\keyword{own}
\keyword{risk}
\keyword{text}
\keyword{use}
\keyword{your}

