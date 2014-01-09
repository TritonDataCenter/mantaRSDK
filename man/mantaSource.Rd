\name{mantaSource}
\alias{mantaSource}
\title{Downloads specified Manta object and applies source() to parse R code file.}
\usage{
  mantaSource(mantapath, local = FALSE, verbose = FALSE,
    max.deparse.length = 150,
    encoding = getOption("encoding"),
    keep.source = getOption("keep.source"))
}
\arguments{
  \item{mantapath}{string, optional. Path to a manta R code
  file or file name in current working Manta directory for
  retrieval.}

  \item{local}{logical optional. See source().}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication on
  stderr which is invisible on Windows}

  \item{max.deparse.length}{optional. See source().}

  \item{encoding}{optional. See source().}

  \item{keep.source}{optional. See source().}
}
\value{
  TRUE or FALSE depending on success of GET transfer
}
\description{
  Downloads specified Manta object and applies source() to
  parse R code file.
}
\keyword{Manta,}
\keyword{manta}

