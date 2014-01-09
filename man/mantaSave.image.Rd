\name{mantaSave.image}
\alias{mantaSave.image}
\title{Workspace Upload function that calls R save.image(); used by mantaSave.ws().}
\usage{
  mantaSave.image(mantapath, md5 = TRUE, headers,
    durability = 2, version = NULL, ascii = FALSE,
    compress = !ascii, info = TRUE, verbose = FALSE)
}
\arguments{
  \item{mantapath}{optional. Path/filename to where
  uploaded data will go on Manta or Manta object/file name
  in current working Manta directory. If no extension is
  provided on the filename, or a non R data style extension
  ".rda" is appended to the end of the filename.}

  \item{md5}{logical. Test md5 hash of R data tempfile
  before/after PUT transfer.}

  \item{headers}{optional. Headers for HTTP transfer, in
  RCurl style. See mantaPut() User metadata headers may be
  provided, E.g.: Avoid supplying the content-type header,
  which is set to the R data type "application/x-r-data",
  and the durability-level header which is handled via the
  durability parameter.}

  \item{durability}{optional. Number of copies to store on
  Manta (2-6). If not provided, uses saved value from
  mantaSetLimits(), system default is 2.}

  \item{ascii}{optional. See save().}

  \item{version}{optional. See save().}

  \item{compress}{optional. See save().}

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
  mantaSave.image uses mantaSave, mantaXfer, which
  implements the RCURL transfer
}
\keyword{Manta,}
\keyword{manta}

