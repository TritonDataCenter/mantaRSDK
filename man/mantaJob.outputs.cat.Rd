\name{mantaJob.outputs.cat}
\alias{mantaJob.outputs.cat}
\title{Retrieves list of Manta output objects given Manta job identifier, then
retrieves each object from Manta and uses cat() to print contents
to the R console.}
\usage{
  mantaJob.outputs.cat(jobid)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default
  uses mantaJobs.tail() to fetch the jobid of the last
  Manta Job run on the service}
}
\description{
  Retrieves list of Manta output objects given Manta job
  identifier, then retrieves each object from Manta and
  uses cat() to print contents to the R console.
}
\keyword{Manta,}
\keyword{manta}

