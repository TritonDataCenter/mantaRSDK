\name{mantaWhoami}
\alias{mantaWhoami}
\title{Returns Manta username currently active}
\usage{
  mantaWhoami(user = TRUE, dc_url = FALSE, key_id = FALSE,
    ssl_key = FALSE, json = FALSE)
}
\arguments{
  \item{user}{logical, optional. TRUE by default to report
  Manta user}

  \item{dc_url}{logical, optional. Set TRUE to get Manta
  data center}

  \item{key_id}{logical, optional. Set TRUE to get the
  current key id}

  \item{ssl_key}{logical, optional. Set TRUE to get the
  private key path}

  \item{json}{logical, optional. Set TRUE to get JSON
  output}
}
\value{
  JSON or R values as specified.
}
\description{
  Report the active Manta account. This is initially
  obtained from environment variables MANTA_USER,
  MANTA_KEY, MANTA_URL. Save account settings, data center
  as JSON with this function. Change/restore account
  settings with mantaAccount and JSON values.
}
\details{
  Save all current settings as JSON with: account <-
  mantaWhoami(dc_url=TRUE, key_id=TRUE, ssl_key=TRUE,
  json=TRUE) and use: mantaAccount(account)

  Get current Manta Datacenter only:
  mantaWhoami(dc_url=TRUE, user=FALSE)
}
\keyword{Manta,}
\keyword{manta}

