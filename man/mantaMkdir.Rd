\name{mantaMkdir}
\alias{mantaMkdir}
\title{Makes a Manta subdirectory, optionally with parent directories.}
\usage{
mantaMkdir(mantapath, p = FALSE, info = TRUE)
}
\arguments{
  \item{mantapath}{character, required. Path or name of new
  subdirectory to create. Not vectorized.}

  \item{p}{logical, optional. Make all the parent
  directories too.}

  \item{info}{logical. Set FALSE to suppress console
  messages.}
}
\value{
\code{TRUE} or \code{FALSE} depending on success of upload.
}
\description{
Makes subdirectory on Manta. Specify absolute (e.g.
\code{~~/stor} ) or relative path from the current Manta
directory. Supports creation of parent directories with
\code{p = TRUE}. New directories can be created in
\code{~~/stor} your private space or \code{~~/public} a
publically accessible HTTPS directory.
}
\examples{
\dontrun{
## Make absolute path subdirectory
mantaMkdir("~~/stor/testdirectory")
mantaLs.l("~~/stor")
mantaRmdir("~~/stor/testdirectory")

## Make with parent directories,
mantaGetwd() -> tempdir
mantaMkdir("~~/stor/a_test/b_test/c_test", p = TRUE)
mantaSetwd("~~/stor/a_test/b_test/c_test")
mantaMkdir("d_test")   # Relative path
data <- runif(100)
mantaDump("data")
mantaSetwd("..")
mantaDump("data")
mantaSetwd("..")
mantaDump("data")
mantaLs.l()
mantaFind()
## Recursive Rm Subdirectory Contents
mantaSetwd.stor()
mantaRm("~~/stor/a_test", r = TRUE)
mantaSetwd("~~/stor/a_test")
mantaLs.l()
mantaFind()
mantaSetwd.stor()
mantaRmdir("~~/stor/a_test")
mantaLs.l("~~/stor")
mantaSetwd(tempdir)
}
}
\seealso{
\code{\link{mantaRmdir}}

Other Directory: \code{\link{mantaGetwd}};
\code{\link{mantaRmdir}}; \code{\link{mantaRm}};
\code{\link{mantaSetwd.public}};
\code{\link{mantaSetwd.reports}};
\code{\link{mantaSetwd.stor}}; \code{\link{mantaSetwd}};
\code{\link{mantaSnapln}}
}
\keyword{Manta}

