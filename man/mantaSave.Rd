\name{mantaSave}
\alias{mantaSave}
\title{Uploads R data to Manta Storage Service using R function \code{save}.}
\usage{
mantaSave(..., list = character(),
  mantapath = stop("'mantapath' destination file or full path must be specified"),
  md5 = FALSE, headers, durability = 2, ascii = FALSE, version = NULL,
  envir = parent.frame(), compress = !ascii, compression_level,
  eval.promises = TRUE, precheck = TRUE, info = TRUE, verbose = FALSE)
}
\arguments{
  \item{...}{See \code{save} R objects to be saved}

  \item{list}{required. See \code{save} List of R objects
  to be saved.}

  \item{mantapath}{required. Path/filename to where
  uploaded data will go on Manta or Manta object/file name
  in current working Manta directory. If no extension is
  provided on the filename, or a non R data style extension
  \code{.rda} is appended to the end of the filename.}

  \item{md5}{logical. Test md5 hash of R data tempfile with
  OpenSSL before/after PUT transfer. Default is
  \code{TRUE}. Setting \code{FALSE} will speed up transfers
  a bit by skipping this step.}

  \item{headers}{optional. Headers for HTTPS transfer, in
  \code{RCurl} style. See \code{\link{mantaPut}}. User
  metadata headers may be provided, E.g.:\cr Avoid
  supplying the \code{content-type} header, which is set to
  the R data type \code{"application/x-r-data"}, and the
  \code{durability-level} header which is handled via the
  \code{durability} parameter.}

  \item{durability}{optional. Number of copies to store on
  Manta (2-6). If not provided, uses saved value from
  \code{mantaSetLimits}, system default is 2.}

  \item{ascii}{optional. See \code{save}.}

  \item{version}{optional. See \code{save}.}

  \item{envir}{optional. See \code{save}. Environment of R
  object being passed.}

  \item{compress}{optional. See \code{save}.}

  \item{compression_level}{optional. See \code{save}.}

  \item{eval.promises}{optional. See \code{save}.}

  \item{precheck}{optional. See \code{save}.}

  \item{info}{logical required. Set to FALSE to silence
  output messages while downloading.}

  \item{verbose}{logical, optional. Passed to \code{RCurl}
  \code{GetURL}, Set to \code{TRUE} to see background REST
  communication on \code{stderr}. Note this is not visible
  on Windows.}
}
\value{
\code{TRUE} or \code{FALSE} depending on success of
transfer.
}
\description{
Uploads to R data files \code{.rda .Rdata .RData} files. If
no file extension is provided, \code{.rda} is appended.
\code{mantaSave} is a wrapper for \code{save} and
\code{mantaXfer}, which implements the RCURL upload.
}
\examples{
\dontrun{
data <- runif(100)
mantaSave("data", mantapath = "~~/stor/data")
rm(data)
mantaExists("~~/stor/data.rda")
mantaLoad("~~/stor/data.rda")
ls()
rm(data)
}
}
\seealso{
\code{\link{mantaLoad}}

Other mantaGet: \code{\link{mantaCat}};
\code{\link{mantaGet}}; \code{\link{mantaLoad.ws}};
\code{\link{mantaLoad}}; \code{\link{mantaSource}}
}
\keyword{Manta}

