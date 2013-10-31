\name{mantaSetwd}
\alias{mantaSetwd}
\title{Sets Manta working directory}
\usage{
  mantaSetwd(mantapath)
}
\arguments{
  \item{mantapath}{string, required.}
}
\description{
  This sets the current working directory in Manta Spaces
  in the directory path are substituted with %20 for HTTP
  i/o. Supports ~~ expansion to $MANTA_USER setting Returns
  FALSE if directory specified incorrectory or if the
  directory does not exist.
}
\keyword{Manta,}
\keyword{manta}

