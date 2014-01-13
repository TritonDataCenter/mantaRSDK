\name{mantaSnapln}
\alias{mantaSnapln}
\title{Makes a Snaplink - combination snapshot and symbolic link.}
\usage{
  mantaSnapln(from, to, info = TRUE)
}
\arguments{
  \item{from}{character, required. Object in current
  subdirectory or full Manta path to stored object.
  Vectorized.}

  \item{to}{character, required. Snaplink name in current
  subdirectory or full Manta object path to the new
  SnapLink. If from is used as vectorized list of
  mantapaths, to must specify a single Manta directory
  ending with / character.}

  \item{info}{logical. When FALSE suppresses messages on
  the console.}
}
\description{
  Example: mantaSnapln(mantaLs(items = 'o'),
  "~~/public/share") will make SnapLinks for all objects in
  the current directory located in the "~~/public/share"
  directory. If the original object is overwritten/deleted,
  the SnapLink still contains the object contents at time
  of creation.
}
\keyword{Manta,}
\keyword{manta}

