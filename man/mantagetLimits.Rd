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
  \item{all}{logical, optional, Get all limits values}

  \item{durability_level}{logical, optional. Get durability
  level.}

  \item{connect_timeout}{logical, optional. Get connect
  timeout.}

  \item{send_timeout,}{logical, optional. Get send
  timeout.}

  \item{receive_timeout,}{logical, optional. Get recieve
  timeout.}

  \item{max_limit}{logical, optional. Get the maximum
  number of directory entries transferred in one HTTPS call
  (upper limit 1000 is the default)}

  \item{json}{logical, optional. Set \code{TRUE} to return
  values in JSON}
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
limits
## Change one or all settings, set with:
mantaSetLimits(limits)
}
}
\seealso{
  \code{\link{mantaSetLimits}}

  Other mantaAccount: \code{\link{mantaAccount}},
  \code{\link{mantaSetLimits}}, \code{\link{mantaWhoami}}
}
\keyword{Manta,}
\keyword{manta}

