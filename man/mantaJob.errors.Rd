\name{mantaJob.errors}
\alias{mantaJob.errors}
\title{mantaJob.errors returns JSON error messages given Manta job identifier}
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
  JSON error message return values Name Type Description
  ------------------------ id String job id phase Number
  phase number of the failure what String a human readable
  summary of what failed code String programmatic error
  code message String human readable error message stderr
  String (optional) a key that saved the stderr for the
  given command key String (optional) the input key being
  processed when the task failed (if the service can
  determine it)
}
\keyword{Manta,}
\keyword{manta}

