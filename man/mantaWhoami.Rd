\name{mantaWhoami}
\alias{mantaWhoami}
\title{Reports the active Manta account information.}
\usage{
  mantaWhoami(all = FALSE, user = TRUE, dc_url = FALSE,
    key_id = FALSE, ssl_key = FALSE, json = FALSE)
}
\arguments{
  \item{all}{logical, optional, \code{TRUE} returns all
  account settings.}

  \item{user}{logical, optional. \code{TRUE} by default to
  report Manta user.}

  \item{dc_url}{logical, optional. Set \code{TRUE} to get
  Manta data center.}

  \item{key_id}{logical, optional. Set \code{TRUE} to get
  the current key id.}

  \item{ssl_key}{logical, optional. Set \code{TRUE} to get
  the private key path.}

  \item{json}{logical, optional. Set \code{TRUE} to get
  JSON output}
}
\value{
  JSON or R values as specified.
}
\description{
  The active Manta account is initially obtained from
  environment variables \code{$MANTA_USER},
  \code{$MANTA_KEY}, and \code{$MANTA_URL}. Retrieve
  account settings, data center as JSON with this function.
  Change/restore account settings with \code{mantaAccount}.
}
\examples{
\dontrun{
mantaWhoami()
## To see/save current account settings:
account <- mantaWhoami(all = TRUE)

## then use:

mantaAccount(account) ## to set the modified account

## Account information may contain 1-4 key-value pairs.

## To see/save current account settings as JSON:

account <- mantaWhoami(all = TRUE, json = TRUE)

## then use:

mantaAccount(json = account) to set that account

}
}
\seealso{
  Other mantaAccount: \code{\link{mantaAccount}},
  \code{\link{mantaGetLimits}},
  \code{\link{mantaSetLimits}}
}
\keyword{Manta,}
\keyword{manta}

