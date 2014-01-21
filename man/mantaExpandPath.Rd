\name{mantaExpandPath}
\alias{mantaExpandPath}
\title{Checks, expands \code{~~} to value of \code{$MANTA_USER}, and applies \code{curlEscape}.}
\usage{
mantaExpandPath(m_path, verbose = FALSE)
}
\arguments{
  \item{m_path}{character, required.}

  \item{verbose}{logical, optional.}
}
\description{
Not exported. Returns \code{""} if subdirectory specified
incorrectory or if the directory cannot exist on the system
as specified.
}
\keyword{Manta}

