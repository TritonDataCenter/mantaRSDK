\name{mantaInit}
\alias{mantaInit}
\title{Initialize Manta and test; use optional private key file}
\usage{
  mantaInit(private_key_path, verbose = FALSE)
}
\arguments{
  \item{private_key_path}{string, optional. Absolute path
  to private SSL key file as registered with user's Joyent
  Manta account. If \code{private_key_path} is not
  provided, looks for default OpenSSL key at
  /$HOME/.ssh/id_rsa}

  \item{verbose}{logical, passed through to RCurl}
}
\value{
  TRUE, otherwise stop() on errors.
}
\description{
  Allows for user-specified SSL key file lookups. Calls
  internal \code{\link{mantaInitialize}}
}
\examples{
mantaInit()
}
\keyword{Manta,}
\keyword{manta}

