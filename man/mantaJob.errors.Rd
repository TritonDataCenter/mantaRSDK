\name{mantaJob.errors}
\alias{mantaJob.errors}
\title{Returns JSON Manta error messages given Manta job identifier.}
\usage{
  mantaJob.errors(jobid, readable = TRUE)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}.
  Default uses \code{mantaJobs.tail} to fetch the jobid of
  the last Manta Job run on the service.}

  \item{readable}{logical. Set to \code{FALSE} to return
  the JSON error strings, or \code{NULL} if no errors
  found. Default \code{TRUE} pretty prints JSON to the
  console.}
}
\description{
  JSON error message return values:\cr \code{Name : Type}.
  Description.\cr \code{------------------------}\cr
  \code{id : String}. Job id.\cr \code{phase : Number}.
  Phase number of the failure.\cr \code{what : String}. A
  human readable summary of what failed.\cr \code{code :
  String}. Programmatic error code.\cr \code{message :
  String}. Human readable error message.\cr \code{stderr :
  String} (optional). A key that saved the stderr for the
  given command.\cr \code{key : String} (optional). The
  input key being processed when the task failed (if the
  service can determine it).\cr
}
\examples{
\dontrun{
## Check if the last run Manta job had errors:
mantaJob.errors()
}
}
\seealso{
  Other mantaJobs: \code{\link{mantaJob.cancel}},
  \code{\link{mantaJob.done}},
  \code{\link{mantaJob.errors.stderr}},
  \code{\link{mantaJob.failures}},
  \code{\link{mantaJob.inputs}},
  \code{\link{mantaJob.outputs}},
  \code{\link{mantaJob.outputs.cat}},
  \code{\link{mantaJob.setup}},
  \code{\link{mantaJob.status}}, \code{\link{mantaJobs}},
  \code{\link{mantaJobs.running}},
  \code{\link{mantaJobs.tail}}
}
\keyword{Manta,}
\keyword{manta}

