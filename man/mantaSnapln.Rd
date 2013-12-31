\name{mantaSnapln}
\alias{mantaSnapln}
\title{Makes Snaplinks - combination snapshots and symbolic links.}
\usage{
  mantaSnapln(from, to)
}
\arguments{
  \item{from}{string, required. Object in current
  subdirectory or full Manta path to stored object.}

  \item{to}{string, required. Snaplink name in current
  subdirectory or full Manta path to the new SnapLink..}
}
\description{
  If the original object is overwritten/deleted, the
  SnapLink still contains the object contents at time of
  creation.
}
\keyword{Manta,}
\keyword{manta}

