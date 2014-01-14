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
  Not exported. Gets system time and converts to web time
  format. Implements HTTPS Signature according to Mark
  Cavage Draft
  \code{http://tools.ietf.org/html/draft-cavage-http-signatures-00}
  using \code{system2} calls to \code{openSSL} binary.
  Under certain conditions, Windows \code{system2} will
  still have a lock on "temp_digest.bin", despite using
  \code{wait = TRUE} generating a permissions Warning on
  the \code{file.remove} command. This function is slated
  for replacement.
}
\keyword{Manta,}
\keyword{manta}

