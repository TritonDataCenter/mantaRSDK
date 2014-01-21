\name{mantaJob.status}
\alias{mantaJob.status}
\title{Returns JSON Manta job status data given Manta job identifier.}
\usage{
mantaJob.status(jobid, readable = TRUE)
}
\arguments{
  \item{readable}{logical. Set to \code{FALSE} to return
  the JSON Job as character(), or \code{NULL} if no Job
  status found. Default \code{TRUE} pretty prints JSON Job
  status to the console.}

  \item{jobid}{character optional. Manta job identifier
  such as \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}.
  Default uses \code{mantaJobs.tail} to fetch the jobid of
  the last Manta Job run on the service.}
}
\description{
Returns JSON Manta job status data given Manta job
identifier.
}
\examples{
\dontrun{
## Retrieve JSON status of the last run Manta job:
mantaJob.status(readable = FALSE) -> status
## Show JSON status of last run Manta job:
mantaJob.status()
}
}
\seealso{
Other mantaJobs: \code{\link{mantaJob.cancel}};
\code{\link{mantaJob.done}};
\code{\link{mantaJob.errors.stderr}};
\code{\link{mantaJob.errors}};
\code{\link{mantaJob.failures}};
\code{\link{mantaJob.inputs}};
\code{\link{mantaJob.launch}};
\code{\link{mantaJob.outputs.cat}};
\code{\link{mantaJob.outputs}};
\code{\link{mantaJob.setup}};
\code{\link{mantaJobs.running}};
\code{\link{mantaJobs.tail}}; \code{\link{mantaJobs}};
\code{\link{mantaMap}}; \code{\link{mantaReduce}}
}
\keyword{Manta}

