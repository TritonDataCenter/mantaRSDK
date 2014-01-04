\name{mantaJob.outputs}
\alias{mantaJob.outputs}
\title{Returns list of output objects given Manta job identifier}
\usage{
  mantaJob.outputs(jobid, silent = FALSE)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default
  uses mantaJobs.tail() to fetch the jobid of the last
  Manta Job run on the service}

  \item{silent}{logical required. Set to TRUE for
  non-interactive use of the function to suppress stop() on
  Manta Service error messages, and return an empty data
  set. N.B. Errors are logged and in the bunyan buffer.}
}
\description{
  Returns list of output objects given Manta job identifier
}
\keyword{Manta,}
\keyword{manta}

