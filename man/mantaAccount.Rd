\name{mantaAccount}
\alias{mantaAccount}
\title{Changes current Manta account information}
\usage{
  mantaAccount(account, json, verbose = TRUE)
}
\arguments{
  \item{account}{list optional. R values}

  \item{json}{character optional. JSON account values}

  \item{verbose,}{logical optional. TRUE by default}
}
\value{
  logical TRUE if account changed, stop on errors.
}
\description{
  The Manta account is initially obtained from environment
  variables $MANTA_USER, $MANTA_KEY, $MANTA_URL. The ssl
  key location is obtained by default on Unix/Linux from
  /$HOME/.ssh/id_rsa OR as set by user with mantaInit().
  The Manta datacentre enviroment variable is $MANTA_URL
}
\details{
  To see/save current account settings:. account <-
  mantaWhoami(dc_url=TRUE, key_id=TRUE, ssl_key=TRUE) then
  use: mantaAccount(account) to set that account

  R account information may contain 1-4 key-value pairs:

  To see/save current account settings as JSON:

  account <- mantaWhoami(dc_url=TRUE, key_id=TRUE,
  ssl_key=TRUE, json=TRUE)

  then use:

  mantaAccount(json=account) to set that account
}
\keyword{Manta,}
\keyword{manta}

