\name{mantaSave.ws}
\alias{mantaSave.ws}
\title{Saves R workspace to Manta ~~/stor/R-3.0.1/ workspace directory
with an audit trail of backups.}
\usage{
  mantaSave.ws()
}
\value{
  logical.
}
\description{
  This function is a wrapper for mantaSave.image that
  handles the versioned default Manta directory, and
  Snaplinks the last current.Rdata to a copy with
  [timestamp].Rdata before it is overwritten.
}
\keyword{Manta,}
\keyword{manta}

