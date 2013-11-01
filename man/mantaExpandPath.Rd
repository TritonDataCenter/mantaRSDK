\name{mantaExpandPath}
\alias{mantaExpandPath}
\title{Checks, expands ~~ and adds blank encoding to subdirectories}
\usage{
  mantaExpandPath(m_path, verbose = FALSE)
}
\arguments{
  \item{m_path}{string, required.}

  \item{verbose}{logical, optional.}
}
\description{
  Spaces in the directory path are substituted with %20 for
  HTTP i/o. Supports ~~ expansion to $MANTA_USER setting
  Returns "" if directory specified incorrectory or if the
  directory does not exist.
}
\keyword{Manta,}
\keyword{manta}

