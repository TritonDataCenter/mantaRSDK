\name{mantaJob.status}
\alias{mantaJob.status}
\title{mantaJob.status returns JSON job status data given Manta job identifier}
\usage{
  mantaJob.status(jobid, readable = TRUE)
}
\arguments{
  \item{jobid}{character required. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4" or use
  mantaJob.last() to fetch the jobid of the last manta Job
  run on the service e.g. mantaJob.errors(mantaJob.last())}

  \item{readable}{logical. Set to FALSE to return the JSON
  Job as character(), or NULL if no Job status found..
  Default TRUE pretty prints JSON Job status to the
  console.}
}
\description{
  mantaJob.status returns JSON job status data given Manta
  job identifier
}
\keyword{Manta,}
\keyword{manta}

