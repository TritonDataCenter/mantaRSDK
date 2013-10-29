\name{mantaAccount}
\alias{mantaAccount}
\title{Changes current Manta account information}
\usage{
  mantaAccount(account, json, verbose = FALSE)
}
\arguments{
  \item{account}{list, optional. R values}

  \item{json}{character, optional. JSON account values}

  \item{verbose}{logical, optional. TRUE by default}
}
\value{
  logical TRUE if account changed and working. Reverts to
  previous working account if it cannot connect wit the new
  information returns FALSE for both cases - account
  reverted or account is left in a state where it cannot
  communicate to the server.
}
\description{
  The Manta account is initially obtained from environment
  variables $MANTA_USER, $MANTA_KEY, $MANTA_URL. The ssl
  key location is obtained by default on Unix/Linux from
  /$HOME/.ssh/id_rsa or on Windows from
  C:\\Users\\username\\.ssh\\ir_rsa The Manta datacentre
  enviroment variable is $MANTA_URL
}
\details{
  To see/save current account settings:

  account <- mantaWhoami(all = TRUE)

  then use:

  mantaAccount(account) to set that account

  Account information may contain 1-4 key-value pairs.

  To see/save current account settings as JSON:

  account <- mantaWhoami(all = TRUE, json = TRUE)

  then use:

  mantaAccount(json = account) to set that account

  To set a non default SSH private key location on Windows

  mysslkey <- list(SSL_KEY_PATH =
  "C:\\Users\\myacct\\.ssh\\my_priv_rsa")
  mantaAccount(mysslkey)
}
\keyword{Manta,}
\keyword{manta}

