\name{mantaJob.errors.stderr}
\alias{mantaJob.errors.stderr}
\title{Retrieves stderr messages given  Manta job identifier.}
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
  When you run a Manta job, any errors that the UNIX
  command attempted that are written to stderr are
  archived. This function retrieves the JSON errors
  messages and then all stderr message archives for each
  process, then uses mantaCat() to print contents of stderr
  to the console.
}
\keyword{Manta,}
\keyword{manta}

