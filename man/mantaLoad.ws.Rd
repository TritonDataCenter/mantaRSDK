\name{mantaLoad.ws}
\alias{mantaLoad.ws}
\title{Loads last current R workspace from Manta R workspace directory.}
\usage{
mantaLoad.ws(envir = parent.frame())
}
\arguments{
  \item{envir}{optional. Environment in which to load, See
  \code{load}.}
}
\value{
\code{TRUE} or \code{FALSE} depending on success of
download.
}
\description{
Downloads Manta \code{"current.Rdata"} object stored in
users' workspace directory containing R workspace and uses
R function \code{load} to load the R workspace.
}
\details{
Together with \code{mantaSave.ws} this function works from
an audit trail of workspaces maintined in a Manta
subdirectory created by \code{mantaSave.ws} made by R
version and mantaRSDK client hostname, that looks like
these:\cr \code{~~/stor/R-3.0.1/cwvh-macbookpro/} \cr
\code{~~/stor/R-3.0.2/CHOGUE-HPDV7/} \cr These workspace
archive subdirectories are made by
\code{\link{mantaSave.ws}}. The last saved R workspace from
these two systems in each directory is named
\code{"current.Rdata"}. Previously saved R workspaces are
renamed to their original GMT creation date/time stamp on
Manta and archived with SnapLinks before writing a new
workspace.\cr Archived workspaces are named like this:\cr
\code{"2014-01-07_14:53:05_GMT.Rdata"} To retrieve an older
workspace or a workspace saved from a different mantaRSDK
client and version, use this form:\cr
\code{mantaLoad("~~/stor/R-3.0.0/hostname/2014-01-07_14:53:05_GMT.Rdata")}
Checks for appropriate \code{content-type} HTTP header,
which is set by \code{mantaSave} or \code{mantaSave.ws} to
\code{"application/x-r-data"}.
}
\examples{
\dontrun{
somedata <- runif(100)
somechar <- "My current workspace"
ls()
mantaSave.ws()
rm(somedata)
rm(somechar)
mantaLoad.ws()
ls()
## What is my workspace subdirectory?
mantaGetwd() -> tempdir
mantaSetwd.ws()
mantaGetwd()   ## this one
mantaLs.l()    ## Inspect workspace archives
mantaSetwd(tempdir)
}
}
\seealso{
\code{\link{mantaSave.ws}} \code{\link{mantaLoad}}
\code{\link{mantaSetwd.ws}}

Other mantaGet: \code{\link{mantaCat}};
\code{\link{mantaGet}}; \code{\link{mantaLoad}};
\code{\link{mantaSave}}; \code{\link{mantaSource}}
}
\keyword{Manta}

