\name{mantaJobs}
\alias{mantaJobs}
\title{Lists all Manta job identifiers, sorted by time.}
\usage{
  mantaJobs()
}
\description{
  Clean out your Manta job directory regularly to avoid
  paying storage costs and having the archive grow to large
  numbers of files.
}
\examples{
\dontrun{
## List all the Manta jobs you have run so far:
mantaJobs()
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
  \code{\link{mantaJob.outputs.cat}},
  \code{\link{mantaJob.setup}},
  \code{\link{mantaJob.status}},
  \code{\link{mantaJobs.running}},
  \code{\link{mantaJobs.tail}}
}
\keyword{Manta,}
\keyword{manta}

