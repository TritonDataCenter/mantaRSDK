\name{mantaSave.image}
\alias{mantaSave.image}
\title{Workspace Upload function that uses R \code{save.image}.}
\usage{
  mantaSave.image(mantapath, md5 = TRUE, headers,
    durability = 2, version = NULL, ascii = FALSE,
    compress = !ascii, info = TRUE, verbose = FALSE)
}
\arguments{
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

  \item{version}{optional. See \code{save}.}

  \item{ascii}{optional. See \code{save}.}

  \item{compress}{optional. See \code{save}.}

  \item{info}{logical required. Set to FALSE to silence
  output messages while downloading.}

  \item{verbose}{logical, optional. Passed to \code{RCurl}
  \code{GetURL}, Set to \code{TRUE} to see background REST
  communication on \code{stderr}. Note this is not visible
  on Windows.}
}
\value{
  \code{TRUE} or \code{FALSE} depending on success of
  upload.
}
\description{
  \code{mantaSave.image} uses \code{mantaSave},
  \code{mantaXfer}, which implements the RCURL transfer
  This function is wrapped by \code{\link{mantaSave.ws}}
  for audit trail management of current and timestamped
  older R workspaces.
}
\examples{
\dontrun{
data <- runif(100)
myusername <- mantaWhoami()
ls()
mantaSave.image(mantapath = "~~/stor/myworkspace")
rm(data)
rm(myusername)
mantaExists("~~/stor/myworkspace.RData")
mantaLoad("~~/stor/myworkspace.RData")
ls()
rm(data)
rm(myusername)
mantaRm("~~/stor/myworkspace.RData")
}
}
\seealso{
  \code{\link{mantaLoad}} \code{\link{mantaSave.ws}}
  \code{\link{mantaLoad.ws}}

  Other mantaPut: \code{\link{mantaDump}},
  \code{\link{mantaPut}}, \code{\link{mantaSave.ws}}
}
\keyword{Manta,}
\keyword{manta}

