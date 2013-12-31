\name{mantaJob.cancel}
\alias{mantaJob.cancel}
\title{mantaJob.cancel Sends Manta a cancel message to stop running job}
\usage{
  mantaJob.cancel(jobid)
}
\arguments{
  \item{jobid}{character required. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4" or use
  mantaJob.last() to fetch the jobid of the last manta Job
  run on the service e.g. mantaJob.errors(mantaJob.last())}
}
\description{
  mantaJob.cancel Sends Manta a cancel message to stop
  running job
}
\keyword{Manta,}
\keyword{manta}

