\name{mantaJob.errors.stderr}
\alias{mantaJob.errors.stderr}
\title{Retrieves stderr messages given  Manta job identifier.}
\usage{
mantaJob.errors.stderr(jobid)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}.
  Default uses \code{mantaJobs.tail} to fetch the jobid of
  the last Manta Job run on the service}
}
\description{
When you run a Manta job, any errors that the UNIX command
attempted that are written to \code{stderr} are archived.
This function retrieves the JSON errors messages and then
all \code{stderr} message archives for each process, then
uses \code{\link{mantaCat}} to print contents of each
\code{stderr} message to the console.
}
\examples{
\dontrun{
# Show all stderr messages emitted by processes of last run Manta job.
mantaJob.errors.stderr()
}
}
\seealso{
Other mantaJobs: \code{\link{mantaJob.cancel}};
\code{\link{mantaJob.done}}; \code{\link{mantaJob.errors}};
\code{\link{mantaJob.failures}};
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

