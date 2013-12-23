\name{mantaDump}
\alias{mantaDump}
\title{Uses dump() to write text parsable  R data to Manta Storage Service}
\usage{
  mantaDump(list, mantapath = "dumpdata.R", md5 = FALSE,
    headers, durability = 2, envir = parent.frame(),
    control = "all", evaluate = TRUE, info = TRUE,
    verbose = FALSE)
}
\arguments{
  \item{list}{required.  See dump() List of R objects to
  dump. Name of R object in quotes works as well.}

  \item{mantapath}{required. Path/filename to where
  uploaded R source will go on Manta or Manta object/file
  name in current working Manta directory. If no extension
  is provided on the filename, or a non R data style
  extension, ".R" is appended to the end of the filename.}

  \item{md5}{logical. Test md5 hash of R dump tempfile
  before/after PUT transfer.}

  \item{headers}{Headers for HTTP transfer, in RCurl style.
  See mantaPut() User metadata headers may be provided,
  E.g.: Avoid supplying the content-type header, which is
  set to the R source code "text/R-code" and the
  durability-level header which is handled via the
  durability parameter.}

  \item{durability}{optional. Number of copies to store on
  Manta (2-6). If not provided, uses saved value from
  mantaSetLimits(), system default is 2.}

  \item{envir}{optional. See dump(). Environment of R
  object being passed.}

  \item{control}{optional.  See dump().}

  \item{evaluate}{optional.  See dump().}

  \item{info}{logical required. Set to FALSE to silence
  output messages while downloading.}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication on
  stderr which is invisible on Windows}
}
\value{
  TRUE or FALSE depending on success of transfer
}
\description{
  mantaDump is a wrapper for dump and mantaXfer, which
  implements the RCURL transfer
}
\keyword{Manta,}
\keyword{manta}

