\name{mantaGenHeaders}
\alias{mantaGenHeaders}
\title{Create all Headers for Manta REST requests}
\usage{
  mantaGenHeaders()
}
\value{
  manta_headers HTTP headers as specified for RCurl
}
\description{
  Creates User Agent, Authorization, Date headers. Calls
  \code{\link{mantaGenSignature}} to generate timestamp and
  authorization signature.
}
\keyword{Manta,}
\keyword{manta}

