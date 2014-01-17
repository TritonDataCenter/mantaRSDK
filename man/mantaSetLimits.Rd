\name{mantaSetLimits}
\alias{mantaSetLimits}
\title{Sets Manta durability level, connection timeouts and limits currently active}
\usage{
  mantaSetLimits(limits, json, verbose = FALSE)
}
\arguments{
  \item{limits}{list, optional. New R limits values.}

  \item{json}{character, optional. New JSON limits values.}

  \item{verbose}{logical, optional. Show HTTPS traffic in
  RCURL and console output.}
}
\value{
  logical \code{TRUE} if values changed \code{FALSE} if
  values unchanged
}
\description{
  Use \code{\link{mantaGetLimits}} to see the settings
  structure and default values. Pass a structure in R or
  JSON to \code{mantaSetLimits} to change values. This is
  where the Manta default connection parameters are
  changed, including the number of copies of an object
  stored on the Manta service "durability_level" which can
  be from 2 to 6, the number of directory entries retrieved
  in one HTTPS call \code{max_limit} which is set to the
  maximum of 1000 by default. The other settings
  \code{recieve_timeout}, \code{sent_timeout} and
  \code{connect_timeout} are for HTTPS transfer sessions
  and are set with values in seconds.
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
  Other mantaAccount: \code{\link{mantaAccount}},
  \code{\link{mantaGetLimits}}, \code{\link{mantaWhoami}}
}
\keyword{Manta,}
\keyword{manta}

