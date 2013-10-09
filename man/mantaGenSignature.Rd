\name{mantaGenSignature}
\alias{mantaGenSignature}
\title{Get time, digest and encrypt for HTTPS authentication}
\usage{
  mantaGenSignature()
}
\value{
  signed time and rsa-sha256 signature
}
\description{
  Gets system time and converts to web time format.
  Implements HTTPS Signature according to Mark Cavage Draft
  http://tools.ietf.org/html/draft-cavage-http-signatures-00
}
\keyword{Manta,}
\keyword{manta}

