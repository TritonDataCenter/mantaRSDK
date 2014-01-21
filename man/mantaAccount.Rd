\name{mantaAccount}
\alias{mantaAccount}
\title{Changes current Manta account information}
\usage{
mantaAccount(account, json, verbose = FALSE)
}
\arguments{
  \item{account}{list, optional. Input R account values.}

  \item{json}{character, optional. Input JSON account
  values.}

  \item{verbose}{logical, optional. Show HTTP
  communication. \code{FALSE} by default.}
}
\value{
logical TRUE if account changed and working. Reverts to
previous working account if it cannot connect wit the new
information returns FALSE for both cases - account reverted
or account is left in a state where it cannot communicate
to the server.
}
\description{
The Manta account is initially obtained from three
environment variables:\cr \code{$MANTA_USER, $MANTA_KEY,
$MANTA_URL}.\cr The ssl key location is obtained by default
on Unix/Linux from\cr \code{/$HOME/.ssh/id_rsa}\cr or on
Windows from:\cr
\code{C:\\Users\\username\\.ssh\\ir_rsa}\cr The Manta
datacentre enviroment variable is:\cr \code{$MANTA_URL}
}
\examples{
\dontrun{
## To see/save current account settings:
account <- mantaWhoami(all = TRUE)

## then use:

mantaAccount(account) ## to set the modified account

## Account information may contain 1-4 key-value pairs.

## To see/save current account settings as JSON:

account <- mantaWhoami(all = TRUE, json = TRUE)

## then use:

mantaAccount(json = account) to set that account

## To set a non default SSH private key location on Windows

mysslkey <- list(SSL_KEY_PATH = "C:\\\\Users\\\\myacct\\\\.ssh\\\\my_priv_rsa")
mantaAccount(mysslkey)
}
}
\seealso{
\code{\link{mantaWhoami}}

Other mantaAccount: \code{\link{mantaGetLimits}};
\code{\link{mantaSetLimits}}; \code{\link{mantaWhoami}}
}
\keyword{Manta}

