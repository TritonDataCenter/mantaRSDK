\name{mantaJob.status}
\alias{mantaJob.status}
\title{Returns JSON Manta job status data given Manta job identifier.}
\usage{
  mantaJob.status(jobid, readable = TRUE)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default
  uses mantaJobs.tail() to fetch the jobid of the last
  Manta Job run on the service}

  \item{readable}{logical. Set to FALSE to return the JSON
  Job as character(), or NULL if no Job status found..
  Default TRUE pretty prints JSON Job status to the
  console.}
}
\description{
  Returns JSON Manta job status data given Manta job
  identifier.
}
\keyword{Manta,}
\keyword{manta}

