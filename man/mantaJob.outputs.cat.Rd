\name{mantaJob.outputs.cat}
\alias{mantaJob.outputs.cat}
\title{Prints contents of all job output objects to the R console.}
\usage{
  mantaJob.outputs.cat(jobid)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}.
  Default uses \code{mantaJobs.tail} to fetch the jobid of
  the last Manta Job run on the service.}
}
\description{
  Avoid using this on binary output data.
}
\examples{
\dontrun{
## Print all the output files to the console.
mantaJob.outputs.cat()
}
}
\seealso{
  Other mantaJobs: \code{\link{mantaJob.cancel}},
  \code{\link{mantaJob.done}},
  \code{\link{mantaJob.errors}},
  \code{\link{mantaJob.errors.stderr}},
  \code{\link{mantaJob.failures}},
  \code{\link{mantaJob.inputs}},
  \code{\link{mantaJob.outputs}},
  \code{\link{mantaJob.setup}},
  \code{\link{mantaJob.status}}, \code{\link{mantaJobs}},
  \code{\link{mantaJobs.running}},
  \code{\link{mantaJobs.tail}}
}
\keyword{Manta,}
\keyword{manta}

