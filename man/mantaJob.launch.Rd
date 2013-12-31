\name{mantaJob.launch}
\alias{mantaJob.launch}
\title{Submits list of input files and R format Manta Job specification.}
\usage{
  mantaJob.launch(inputs, job, inputbatch = 500,
    verbose = TRUE)
}
\arguments{
  \item{inputs,}{optional. List of inputs as a vector of
  character, each containing valid paths to Manta objects
  that are the intended job input files. You may use output
  from mantaFind() or mantaLs.paths() here.}

  \item{job,}{required. The R job structure as created with
  mantaJob.Setup() and Map and Reduce job tasks as defined
  therein by one or more mantaMap() and/or mantaReduce()
  steps. More information and parameters are explained in
  the help for these three functions.}

  \item{verbose}{logical optional. Passed to RCURL to
  reveal HTTP communication.}

  \item{inputbatch}{numeric. Maximum number of input object
  paths to upload in one batch to the running job. Default
  is 500.}
}
\value{
  Returns a Manta Job identifier, which is a hash that
  looks like this: "70c30bab-873b-66da-ebc8-ced12bd35ac4".
  This is used by other mantaJob functions for information,
  error and output retrieval as a lookup key. This key can
  also be used by Node.js Manta command-line mjob commands.
}
\description{
  Job is created by mantaJob.setup() and tasks as defined
  therein by mantaMap(), and/or mantaReduce() functions.
  Note that Manta tasks are UNIX shell commands, not native
  R commands.
}
\details{
  Example - Manta Map/Reduce UNIX Word Count:

  jobid <- mantaJob.launch( inputs =
  mantaLs.paths("~~/public/shakespeare", grepfor =
  "[.]txt"), job = mantaJob.setup( name = "word count",
  mantaMap("wc"), mantaReduce("awk '{ l += $1; w += $2; c
  += $3 } END { print l, w, c }'") ) )

  mantaJob.status(jobid) # check to see if job is complete
  mantaJob.output(jobid) # retrieve list of paths to Manta
  output objects mantaJob.errors(jobid) # retrirve JSON
  formatted job error information
}
\keyword{Manta,}
\keyword{manta}

