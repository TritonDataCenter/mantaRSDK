\name{mantaJob.outputs}
\alias{mantaJob.outputs}
\title{Returns list of output Manta objects given Manta job identifier.}
\usage{
  mantaJob.outputs(jobid, silent = FALSE)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}.
  Default uses \code{mantaJobs.tail} to fetch the jobid of
  the last Manta Job run on the service.}

  \item{silent}{logical required. Set to \code{TRUE} for
  non-interactive use of the function to suppress
  \code{stop} on Manta Service error messages, and return
  an empty data set. N.B. Errors are logged and in the
  bunyan buffer.}
}
\description{
  Outputs have hashed file names as saved by the Manta
  service.
}
\examples{
\dontrun{
## List the outputs of the last job run on Manta:
mantaJob.outputs()
## Download the output files of the last job run on Manta, to current R working
## directory, keeping hashed filenames.
mantaGet(mantaJob.outputs())
}
}
\seealso{
  Other mantaJobs: \code{\link{mantaJob.cancel}},
  \code{\link{mantaJob.done}},
  \code{\link{mantaJob.errors}},
  \code{\link{mantaJob.errors.stderr}},
  \code{\link{mantaJob.failures}},
  \code{\link{mantaJob.inputs}},
  \code{\link{mantaJob.outputs.cat}},
  \code{\link{mantaJob.setup}},
  \code{\link{mantaJob.status}}, \code{\link{mantaJobs}},
  \code{\link{mantaJobs.running}},
  \code{\link{mantaJobs.tail}}
}
\keyword{Manta,}
\keyword{manta}

