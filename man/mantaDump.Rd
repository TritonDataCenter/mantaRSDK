\name{mantaDump}
\alias{mantaDump}
\title{Uses \code{dump} to upload text parsable R data to Manta Storage Service.}
\usage{
  mantaDump(list, mantapath = "dumpdata.R", md5 = FALSE,
    headers, durability = 2, envir = parent.frame(),
    control = "all", evaluate = TRUE, info = TRUE,
    verbose = FALSE)
}
\arguments{
  \item{list}{required.  See \code{dump}. List of R objects
  to dump. Name of R object in quotes works as well.}

  \item{mantapath}{optional. Default is \code{dumpdata.R}.
  Specify full Manta path to where uploaded R source will
  be saved or Manta object name in current working Manta
  directory. If no extension is provided on the filename,
  or a non R data style extension, \code{.R} is appended to
  the end of the filename.}

  \item{md5}{logical. Test \code{md5} hash of R dump
  tempfile before/after upload.}

  \item{headers}{Headers for HTTP transfer, in \code{RCurl}
  style. See \code{\link{mantaPut}}. \cr User metadata
  headers may be provided, E.g.:\cr Avoid supplying the
  \code{content-type} header, which is set to the R source
  code \code{text/R-code} and the \code{durability-level}
  header which is handled via the \code{durability}
  parameter.}

  \item{durability}{optional. Number of copies to store on
  Manta (2-6). If not provided, uses saved value from
  \code{\link{mantaSetLimits}}, system default is 2.}

  \item{envir}{optional. See \code{dump}. Environment of R
  object being passed.}

  \item{control}{optional.  See \code{dump}.}

  \item{evaluate}{optional.  See \code{dump}.}

  \item{info}{logical required. Set to \code{FALSE} to
  silence console output messages.}

  \item{verbose}{logical, optional. Passed to \code{RCurl}
  \code{GetURL}, Set to \code{TRUE} to see background HTTPS
  REST communication on \code{stderr} Note this is not
  visible on Windows.}
}
\value{
  \code{TRUE} or \code{FALSE} depending on success of
  upload.
}
\description{
  \code{mantaDump} is a wrapper for \code{dump} and
  \code{mantaXfer}, which implements the RCURL transfer
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
  \code{\link{mantaSource}}

  Other mantaPut: \code{\link{mantaSave.ws}}
}
\keyword{Manta,}
\keyword{manta}

