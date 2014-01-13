\name{mantaJob.errors}
\alias{mantaJob.errors}
\title{Returns JSON Manta error messages given Manta job identifier.}
\usage{
  mantaJob.errors(jobid, readable = TRUE)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default
  uses mantaJobs.tail() to fetch the jobid of the last
  Manta Job run on the service}

  \item{readable}{logical. Set to FALSE to return the JSON
  error strings, or NULL if no errors found.. Default TRUE
  pretty prints JSON to the console.}
}
\description{
  Returns JSON Manta error messages given Manta job
  identifier.
}
\keyword{(if}
\keyword{(optional).}
\keyword{------------------------}
\keyword{A}
\keyword{Description\cr}
\keyword{Human}
\keyword{JSON}
\keyword{Job}
\keyword{Manta,}
\keyword{Name}
\keyword{Number.}
\keyword{Phase}
\keyword{Programmatic}
\keyword{String}
\keyword{String.}
\keyword{The}
\keyword{Type}
\keyword{being}
\keyword{can}
\keyword{code:}
\keyword{code\cr}
\keyword{command\cr}
\keyword{determine}
\keyword{error}
\keyword{failed}
\keyword{failed\cr}
\keyword{failure\cr}
\keyword{for}
\keyword{given}
\keyword{human}
\keyword{id:}
\keyword{id\cr}
\keyword{input}
\keyword{it)\cr}
\keyword{key}
\keyword{key:}
\keyword{manta}
\keyword{message}
\keyword{message:}
\keyword{message\cr}
\keyword{number}
\keyword{of}
\keyword{phase:}
\keyword{processed}
\keyword{readable}
\keyword{return}
\keyword{saved}
\keyword{service}
\keyword{stderr}
\keyword{stderr:}
\keyword{summary}
\keyword{task}
\keyword{that}
\keyword{the}
\keyword{values:\cr}
\keyword{what}
\keyword{what:}
\keyword{when}

