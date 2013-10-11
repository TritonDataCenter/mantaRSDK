\name{mantaPath}
\alias{mantaPath}
\title{Given a manta object, returns full path}
\usage{
  mantaPath(m_object, check = FALSE)
}
\arguments{
  \item{m_object}{string, required. Manta object}

  \item{check}{logical, optional. Set to TRUE to verify
  that object exists on storage, returns empty string if
  absent.}
}
\value{
  String with full Manta path to object.
}
\description{
  Given a manta object, returns full path
}
\keyword{Manta,}
\keyword{manta}

