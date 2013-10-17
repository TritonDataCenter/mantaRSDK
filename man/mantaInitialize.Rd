\name{mantaInitialize}
\alias{mantaInitialize}
\title{Initialize Manta user variables, check SSL key file exists}
\usage{
  mantaInitialize(useEnv = TRUE)
}
\arguments{
  \item{useEnv}{logical: TRUE unless called from
  mantaAccount - skips getting env/system settings}
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

