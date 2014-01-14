\name{mantaJob.setup}
\alias{mantaJob.setup}
\title{Constructor for R format Manta Job including \code{name}, and UNIX command line
tasks as defined by
\code{mantaMap}, and/or \code{mantaReduce} functions.}
\usage{
  mantaJob.setup(name, ...)
}
\arguments{
  \item{name}{character, optional.}

  \item{...}{One or more \code{mantaMap} \code{mantaReduce}
  functions with arguments listed in order of task
  execution.}
}
\description{
  Function to construct R structure for
  \code{\link{mantaJob.launch}}. Specify a \code{name} for
  the Manta job and tasks to execute via one or more calls
  to \code{\link{mantaMap}} and/or
  \code{\link{mantaReduce}} which define and parameterize
  each task.
}
\details{
  mantaJob.setup combines multiple tasks into a job
  pipeline structure for \code{\link{mantaJob.launch}}. See
  \code{\link{mantaMap}} and \code{\link{mantaReduce}} for
  parameter details. Their \code{exec} parameter must be a
  valid generic UNIX command line, not an R function.
}
\examples{
\dontrun{
##  Map/Reduce Unix Word Count Job description
job <- mantaJob.setup("Word Count",
  mantaMap("wc"),
  mantaReduce("awk '\\{ l += $1; w += $2; c += $3 \\} END \\{ print l, w, c \\}'"))
## Launch the Job with some text files as input:
inputs <- mantaLs.paths("~~/stor/shakespeare", grepfor = "[.]txt")
mantaJob.launch(inputs, job)
## Check output
mantaJob.outputs()
## Check errors
mantaJob.errors()
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
  \code{\link{mantaJob.status}}, \code{\link{mantaJobs}},
  \code{\link{mantaJobs.running}},
  \code{\link{mantaJobs.tail}}
}
\keyword{Manta,}
\keyword{manta}

