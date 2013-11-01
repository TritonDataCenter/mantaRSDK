\name{mantaSnapln}
\alias{mantaSnapln}
\title{Snaplinks are combination snapshots and symbolic links.
If the original object is overwritten/deleted, the SnapLink
still contains the object contents at time of creation.}
\usage{
  mantaSnapln(from, to)
}
\arguments{
  \item{from}{string, requireed. Object in current
  subdirectory or full Manta path to stored object.}

  \item{to}{string, requireed. Snaplink name in current
  subdirectory or full Manta path to the new SnapLink..}
}
\description{
  Snaplinks are combination snapshots and symbolic links.
  If the original object is overwritten/deleted, the
  SnapLink still contains the object contents at time of
  creation.
}
\keyword{Manta,}
\keyword{manta}

