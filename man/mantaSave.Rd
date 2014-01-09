\name{mantaSave}
\alias{mantaSave}
\title{Uploads R data to Manta Storage Service using R function save().}
\usage{
  mantaSave(..., list = character(),
    mantapath = stop("'mantapath' destination file or full path must be specified"),
    md5 = FALSE, headers, durability = 2, ascii = FALSE,
    version = NULL, envir = parent.frame(),
    compress = !ascii, compression_level,
    eval.promises = TRUE, precheck = TRUE, info = TRUE,
    verbose = FALSE)
}
\arguments{
  \item{...}{See save() R objects to be saved}

  \item{list}{required. See save() List of R objects to be
  saved.}

  \item{mantapath}{required. Path/filename to where
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

  \item{envir}{optional. See save(). Environment of R
  object being passed.}

  \item{compress}{optional. See save().}

  \item{compression_level}{optional. See save().}

  \item{eval.promises}{optional. See save().}

  \item{precheck}{optional. See save().}

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
  To save an R object,
}
\details{
  mantaSave is a wrapper for save and mantaXfer, which
  implements the RCURL transfer
}
\keyword{Manta,}
\keyword{manta}

