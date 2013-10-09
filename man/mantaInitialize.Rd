\name{mantaInitialize}
\alias{mantaInitialize}
\title{Initialize Manta user variables, check SSL key file exists}
\usage{
  mantaInitialize(pk)
}
\arguments{
  \item{pk}{Absolute path to user's private SSL key file as
  registered with Joyent Manta account \code{pk}}
}
\value{
  TRUE or stop() on errors: missing env variables, SSL key
}
\description{
  Initialization of manta_globals with environment
  variables checks SSL private key file, sets manta cwd to
  ~~/stor
}
\keyword{Manta,}
\keyword{manta}

