\name{mantaJob.cancel}
\alias{mantaJob.cancel}
\title{Sends Manta a cancel message to stop running job}
\usage{
  mantaJob.cancel(jobid)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default
  uses mantaJobs.tail() to fetch the jobid of the last
  Manta Job run on the service}
}
\description{
  Sends Manta a cancel message to stop running job
}
\keyword{Manta,}
\keyword{manta}

