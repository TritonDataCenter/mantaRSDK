\name{mantaSetLimits}
\alias{mantaSetLimits}
\title{Sets Manta durability level, connection timeouts and limits currently active}
\usage{
  mantaSetLimits(limits, json, verbose = FALSE)
}
\arguments{
  \item{limits}{list, optional. R values}

  \item{json}{character, optional. JSON limits values}

  \item{verbose}{logical, optional. TRUE by default}
}
\value{
  logical TRUE if values changed FALSE if values unchanged
}
\description{
  Use mantaGetLimits to see the settings structure and
  default values. Pass a structure in R or JSON to this
  function to change values. This is where the Manta
  default connection parameters are changed, including the
  number of copies of an object stored on the Manta service
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

