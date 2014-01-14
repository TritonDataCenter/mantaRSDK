\name{mantaLoad}
\alias{mantaLoad}
\title{Downloads specified Manta object containing R data and uses R function \code{load}.}
\usage{
  mantaLoad(mantapath, envir = parent.frame(), info = TRUE,
    verbose = FALSE)
}
\arguments{
  \item{mantapath}{character, optional. Path to a manta R
  data file or file name in current working Manta directory
  for retrieval. Not vectorized.}

  \item{envir}{optional. Environment in which to load, See
  \code{load}.}

  \item{info}{optional. Print information messages to
  console.}

  \item{verbose}{logical, optional. Passed to \code{RCurl}
  \code{GetURL}, Set to \code{TRUE} to see background REST
  communication on \code{stderr} Note this is invisible on
  Windows.}
}
\value{
  \code{TRUE} or \code{FALSE} depending on success of
  download.
}
\description{
  Used to download \code{.rda} \code{.Rdata} files and
  \code{load} their R data into the workspace or specified
  \code{envir}.
}
\details{
  Checks for appropriate \code{content-type} HTTP header,
  which is set by \code{mantaSave} or \code{mantaSave.ws}
  to \code{"application/x-r-data"}.
}
\examples{
\dontrun{
somedata <- runif(100)
ls()
mantaSave("somedata", mantapath = "~~/stor/somedata.rda")
rm(somedata)
mantaLoad("somedata.rda")
ls()
}
}
\seealso{
  \code{\link{mantaSave}}

  Other mantaGet: \code{\link{mantaCat}},
  \code{\link{mantaGet}}, \code{\link{mantaLoad.ws}}
}
\keyword{Manta,}
\keyword{manta}

