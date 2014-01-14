\name{mantaJob.done}
\alias{mantaJob.done}
\title{Checks or polls status of a Manta job. Returns done or not as logical.}
\usage{
  mantaJob.done(jobid, poll = FALSE, sleep = 30,
    timeout = 600, silent = FALSE)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}.
  Default uses \code{mantaJobs.tail} to fetch the jobid of
  the last Manta Job run on the service}

  \item{poll}{logical. Set to \code{TRUE} to poll. Returns
  \code{FALSE} when poll timeout exceeded and job still
  running, \code{TRUE} when job finished.}

  \item{sleep}{integer. Sleep interval used when polling.
  Default is 30 seconds.}

  \item{timeout}{integer. Seconds after which function
  stops polling. Default is 600 seconds.}

  \item{silent}{logical required. Set to \code{TRUE} for
  non-interactive use of the function.  N.B. Errors are
  logged to the bunyan buffer.}
}
\value{
  \code{TRUE} when job is no longer running.\cr
  \code{FALSE} when job running.\cr \code{NULL} if job
  status not found.\cr N.B. \code{TRUE} return does not
  imply job success/failure/errors, only running/done
  state.
}
\description{
  Checks or polls status of a Manta job. Returns done or
  not as logical.
}
\examples{
\dontrun{
## Test if last run job is done
mantaJob.done()
## Poll a running job till done or timed out.
mantaJob.done(poll = TRUE, sleep = 10, timeout = 60)
}
}
\seealso{
  Other mantaJobs: \code{\link{mantaJob.cancel}},
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

