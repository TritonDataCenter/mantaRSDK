\name{mantaJob.done}
\alias{mantaJob.done}
\title{Checks or polls status of a Manta job. Returns done or not as logical.}
\usage{
  mantaJob.done(jobid, poll = FALSE, sleep = 30,
    timeout = 600, silent = FALSE)
}
\arguments{
  \item{jobid}{character optional. Manta job identifier
  such as "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default
  uses mantaJobs.tail() to fetch the jobid of the last
  Manta Job run on the service}

  \item{poll}{logical. Set to TRUE to poll. Returns FALSE
  when poll timeout exceeded and job still running, TRUE
  when job finished.}

  \item{sleep}{integer. Sleep interval used when polling.
  Default is 30 seconds}

  \item{timeout}{integer. Seconds after which function
  stops polling. Default is 600 seconds.}

  \item{silent}{logical required. Set to TRUE for
  non-interactive use of the function.  N.B. Errors are
  logged and in the bunyan buffer.}
}
\description{
  TRUE when job is no longer running.  FALSE when job
  running. NULL if job status not found. N.B. TRUE return
  does not imply job success/failure/errors, only
  running/done state.
}
\keyword{Manta,}
\keyword{manta}

