\name{mantaWhoami}
\alias{mantaWhoami}
\title{Returns Manta username currently active}
\usage{
  mantaWhoami(all = FALSE, user = TRUE, dc_url = FALSE,
    key_id = FALSE, ssl_key = FALSE, json = FALSE)
}
\arguments{
  \item{all}{logical, optional, TRUE returns all account
  settings}

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
  Save all current settings with:

  account <- mantaWhoami(all = TRUE)

  Restore that account with:

  mantaAccount(account)

  Get current Manta Datacenter only, as JSON:
  mantaWhoami(dc_url=TRUE, user=FALSE, json=TRUE)
}
\keyword{Manta,}
\keyword{manta}

