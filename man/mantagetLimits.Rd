\name{mantaGetLimits}
\alias{mantaGetLimits}
\title{Returns Manta durability level, connection timeouts and limits currently active.}
\usage{
  mantaGetLimits(all = TRUE, durability_level = FALSE,
    connect_timeout = FALSE, send_timeout = FALSE,
    receive_timeout = FALSE, max_limit = FALSE,
    json = FALSE)
}
\arguments{
  \item{all}{logical, optional, TRUE by default all limit
  settings}

  \item{durability_level}{logical, optional. Set TRUE to
  retrive}

  \item{connect_timeout}{logical, optional. Set TRUE to
  retrive}

  \item{send_timeout,}{logical, optional. Set TRUE to
  retrieve}

  \item{receive_timeout,}{logical, optional. Set TRUE to
  retrive}

  \item{max_limit}{logical, optional. Set TRUE to get the
  max dir limit}

  \item{json}{logical, optional. Set TRUE to get JSON
  output}
}
\value{
  JSON or R values as specified.
}
\description{
  Use mantaGetLimits to see the settings structure and
  default values. This is where the Manta default
  connection parameters are changed, including the number
  of copies of an object stored on the Manta service
  "durability_level" which can be from 2 to 6, the number
  of directory entries retrieved in one http call
  "max_limit" which is set to the maximum of 1000 by
  default. The other settings recieve_timeout, sent_timeout
  and connect_timeout are for http transfer sessions and
  are set with values in seconds.
}
\details{
  Save all current settings with:

  limits <- mantaGetLimits(all = TRUE)

  Change one or all settings, set with:

  mantaSetLimits(limits)
}
\keyword{Manta,}
\keyword{manta}

