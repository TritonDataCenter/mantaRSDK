\name{mantaJob.cancel}
\alias{mantaJob.cancel}
\title{Sends Manta a cancel message to stop running job.}
\usage{
  mantaJob.cancel(jobid)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}.
  Default uses \code{mantaJobs.tail} to fetch the jobid of
  the last Manta Job run on the service.}
}
\description{
  Sends Manta a cancel message to stop running job.
}
\examples{
\dontrun{
# Send cancel signal for last run Manta job.
mantaJob.cancel()
}
}
\seealso{
  Other mantaJobs: \code{\link{mantaJob.done}},
  \code{\link{mantaJob.errors}},
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

