\name{mantaSetLimits}
\alias{mantaSetLimits}
\title{Sets Manta connection timeouts and limits currently active}
\usage{
  mantaSetLimits(limits, json, verbose = FALSE)
}
\arguments{
  \item{limits}{list, optional. R values}

  \item{json}{character, optional. JSON limits values}

  \item{verbose}{logical, optional. TRUE by default}
}
\value{
  logical TRUE if values changed FALSE if values unchanged
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

