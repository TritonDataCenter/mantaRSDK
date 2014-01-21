\name{mantaSetwd}
\alias{mantaSetwd}
\title{Sets current working directory on Manta.}
\usage{
mantaSetwd(mantapath)
}
\arguments{
  \item{mantapath}{character, required. Absolute or
  relative subdirectory name to set to.}
}
\description{
This sets the current working directory in Manta. Supports
\code{~~} expansion to \code{$MANTA_USER} setting, i.e. for
my account \code{~~/stor} expands to \code{/cwvhogue/stor}.
There are 4 top level Manta subdirectories:\cr
\code{~~/stor} Your private storage.\cr \code{~~/public}
Your public storage.\cr \code{~~/jobs} Your job archive.\cr
\code{~~/reports} Your account report information.\cr
Specify the full path (absolute) or start from current
working directory (relative). All mantaRSDK functions
assume unprefixed filenames are in the current working
directory on Manta. To move UP one directory at a time use
\code{".."} but note that \code{"../.."} forms are NOT
supported as there are no parent directory \code{".."}
object links on Manta. Returns FALSE if directory specified
incorrectory or if the directory does not exist. The
current working directory is stored internally in
\code{mantaRSDK} on your local system and is not saved.
between sessions. It initializes to the root directory of
private Manta storage: \code{~~/stor}.
}
\examples{
\dontrun{
## Show current Manta working directory
mantaGetwd()
## Save current subdirectory
mantaGetwd() -> tempdir
## Absolute path with ~~ expansion
mantaSetwd("~~/public")
mantaGetwd()
## Dotted forms for 4 top level subdirectories:
mantaSetwd.public()
mantaGetwd()
mantaLs.l()
mantaSetwd.stor()
mantaGetwd()
mantaLs.l()
mantaSetwd.jobs()
mantaGetwd()
mantaLs.l()
mantaSetwd.reports()
mantaGetwd()
mantaLs.l()
## Restore saved subdirectory
mantaSetwd(tempdir)
}
}
\seealso{
Other Directory: \code{\link{mantaGetwd}};
\code{\link{mantaMkdir}}; \code{\link{mantaRmdir}};
\code{\link{mantaRm}}; \code{\link{mantaSetwd.public}};
\code{\link{mantaSetwd.reports}};
\code{\link{mantaSetwd.stor}}; \code{\link{mantaSnapln}}
}
\keyword{Manta}

