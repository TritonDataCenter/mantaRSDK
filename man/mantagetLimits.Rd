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
  Reports the mantaRSDK settings structure and
  default/current values. Includes the number of copies of
  an object stored on the Manta service
  \code{durability_level} which can be from 2 to 6, the
  number of directory entries retrieved in one HTTPS call
  \code{max_limit} set to the maximum of 1000 by default.
  The other settings \code{recieve_timeout, sent_timeout}
  and \code{connect_timeout} are for HTTPS transfer
  sessions and are set with values in seconds.
}
\examples{
\dontrun{
## Save all current settings with:
limits <- mantaGetLimits(all = TRUE)
## Change one or all settings, set with:
mantaSetLimits(limits)
}
}
\seealso{
  \code{\link{mantaSetLimits}}

  Other mantaAccount: \code{\link{mantaAccount}}
}
\keyword{Manta,}
\keyword{manta}

