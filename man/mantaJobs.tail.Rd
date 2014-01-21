\name{mantaJobs.tail}
\alias{mantaJobs.tail}
\title{Returns identifier of last run Manta job identifier,
or from offset \code{n} up from end of list.}
\usage{
mantaJobs.tail(n = 1)
}
\arguments{
  \item{n}{integer. Index into most recently run Jobs list.
  \code{n = 1} default is last Manta Job run. \code{n = 2}
  is second last, and so on.}
}
\description{
Returns identifier of last run Manta job identifier, or
from offset \code{n} up from end of list.
}
\examples{
\dontrun{
## Get the Job Id of the last run Manta job:
mantaJobs.tail()
## Fifth last Manta job id:
mantaJobs.tail(n = 5)
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
\code{\link{mantaJob.status}};
\code{\link{mantaJobs.running}}; \code{\link{mantaJobs}};
\code{\link{mantaMap}}; \code{\link{mantaReduce}}
}
\keyword{Manta}

