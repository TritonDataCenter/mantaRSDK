\name{mantaJob.errors}
\alias{mantaJob.errors}
\title{Returns JSON Manta error messages given Manta job identifier}
\usage{
  mantaJob.errors(jobid, readable = TRUE)
}
\arguments{
  \item{jobid}{character required. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4" or use
  mantaJob.last() to fetch the jobid of the last manta Job
  run on the service e.g. mantaJob.errors(mantaJob.last())}

  \item{readable}{logical. Set to FALSE to return the JSON
  error strings, or NULL if no errors found.. Default TRUE
  pretty prints JSON to the console.}
}
\description{
  Returns JSON Manta error messages given Manta job
  identifier
}
\keyword{Manta,}
\keyword{manta}

