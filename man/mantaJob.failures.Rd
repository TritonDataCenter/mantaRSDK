\name{mantaJob.failures}
\alias{mantaJob.failures}
\title{Returns list of failures given Manta job identifier.}
\usage{
  mantaJob.failures(jobid)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default
  uses mantaJobs.tail() to fetch the jobid of the last
  Manta Job run on the service}
}
\description{
  Returns list of failures given Manta job identifier.
}
\keyword{Manta,}
\keyword{manta}

