\name{mantaPath}
\alias{mantaPath}
\title{Given a user typed path or object name, returns full path,
does not validate object is there, assumes it is in current subdir.
Not exported.}
\usage{
  mantaPath(m_path)
}
\arguments{
  \item{m_path}{character, required. Manta object}
}
\value{
  character with full Manta path to object or empty string
  "" as processed by curlEscape with / left in
}
\description{
  Given a user typed path or object name, returns full
  path, does not validate object is there, assumes it is in
  current subdir. Not exported.
}
\keyword{Manta,}
\keyword{manta}

