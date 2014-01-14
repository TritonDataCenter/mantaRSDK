\name{mantaExists}
\alias{mantaExists}
\title{Tests to see if a Manta object or subdirectory exists.}
\usage{
  mantaExists(mantapath, d = FALSE)
}
\arguments{
  \item{mantapath}{character, required. A full path
  specifying a Manta object or directory, or the name of an
  object/subdir in the current Manta working directory.}

  \item{d}{logical. Set \code{TRUE} to confirm that entity
  specified in mantapath exists and is a directory.}
}
\value{
  logical.
}
\description{
  Like a Unix \code{stat} command. Does not download object
  contents.
}
\examples{
\dontrun{
data <- runif(100)
mantaDump("data")
mantaExists("dumpdata.R")
mantaRm("dumpdata.R")
mantaExists("dumpdata.R")
mantaMkdir("testsubidrectory")
mantaExists("testsubdirectory", d = TRUE)
mantaRmdir("testsubdirectory")
mantaExists("testsubdirectory", d = TRUE)
files <- c("file1", "file2", "file3")
sapply(files, mantaExists)
}
}
\seealso{
  Other mantaLs: \code{\link{mantaLs}},
  \code{\link{mantaLs.du}}, \code{\link{mantaLs.l}},
  \code{\link{mantaLs.n}}, \code{\link{mantaLs.paths}},
  \code{\link{mantaLs.url}}
}
\keyword{Manta,}
\keyword{manta}

