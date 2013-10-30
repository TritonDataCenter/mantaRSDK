\name{mantaGetLimits}
\alias{mantaGetLimits}
\title{Returns Manta connection timeouts and limits  currently active}
\usage{
  mantaGetLimits(all = TRUE, connect_timeout = FALSE,
    send_timeout = FALSE, receive_timeout = FALSE,
    max_limit = FALSE, json = FALSE)
}
\arguments{
  \item{all}{logical, optional, TRUE by default all limit
  settings}

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
  Save all current settings with:
}
\details{
  limits <- mantaGetLimits(all = TRUE)

  Change one or all settings, set with:

  mantaSetLimits(limits)
}
\keyword{Manta,}
\keyword{manta}

