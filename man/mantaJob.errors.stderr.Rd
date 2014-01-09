\name{mantaJob.errors.stderr}
\alias{mantaJob.errors.stderr}
\title{Retrieves JSON errors given Manta job identifier, then retrieves each stderr message
archived on Manta (if any) and uses mantaCat() to print contents of stderr
to the console.}
\usage{
  mantaJob.errors.stderr(jobid)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default
  uses mantaJobs.tail() to fetch the jobid of the last
  Manta Job run on the service}
}
\description{
  Retrieves JSON errors given Manta job identifier, then
  retrieves each stderr message archived on Manta (if any)
  and uses mantaCat() to print contents of stderr to the
  console.
}
\keyword{Manta,}
\keyword{manta}

