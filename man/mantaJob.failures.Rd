\name{mantaJob.failures}
\alias{mantaJob.failures}
\title{Returns list of failures given Manta job identifier.}
\usage{
mantaJob.failures(jobid)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}.
  Default uses \code{mantaJobs.tail} to fetch the jobid of
  the last Manta Job run on the service.}
}
\description{
Returns list of failures given Manta job identifier.
}
\examples{
\dontrun{
## Check if the last run Manta job had failures:
mantaJob.failures()
}
}
\seealso{
Other mantaJobs: \code{\link{mantaJob.cancel}};
\code{\link{mantaJob.done}};
\code{\link{mantaJob.errors.stderr}};
\code{\link{mantaJob.errors}};
\code{\link{mantaJob.inputs}};
\code{\link{mantaJob.launch}};
\code{\link{mantaJob.outputs.cat}};
\code{\link{mantaJob.outputs}};
\code{\link{mantaJob.setup}};
\code{\link{mantaJob.status}};
\code{\link{mantaJobs.running}};
\code{\link{mantaJobs.tail}}; \code{\link{mantaJobs}};
\code{\link{mantaMap}}; \code{\link{mantaReduce}}
}
\keyword{Manta}

