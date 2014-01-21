\name{mantaGenHeaders}
\alias{mantaGenHeaders}
\title{Create Headers for Manta HTTPS REST requests}
\usage{
mantaGenHeaders()
}
\value{
manta_headers HTTP headers as specified for \code{RCurl}
}
\description{
Not exported. Creates \code{User-Agent, Authorization,
date} headers. Calls \code{\link{mantaGenSignature}} to
generate \code{date} and \code{Authorization} signature
using \code{openSSL}.
}
\keyword{Manta}

