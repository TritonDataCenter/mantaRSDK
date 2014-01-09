\name{mantaSetwd}
\alias{mantaSetwd}
\title{Sets Manta working directory.}
\usage{
  mantaSetwd(mantapath)
}
\arguments{
  \item{mantapath}{string, required.}
}
\description{
  This sets the current working directory in Manta Supports
  ~~ expansion to $MANTA_USER setting Specify the full path
  or start from current working directory Specify ".." to
  move up ONE directory at a time. Returns FALSE if
  directory specified incorrectory or if the directory does
  not exist.
}
\keyword{Manta,}
\keyword{manta}

