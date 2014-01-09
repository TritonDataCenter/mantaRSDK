\name{mantaJob.launch}
\alias{mantaJob.launch}
\title{Submits list of input Manta objects and R format Manta Job specification, runs job
optionally polls job status. Returns job status.}
\usage{
  mantaJob.launch(inputs, job, batchsize = 500,
    watch = TRUE, sleep = 30, watchtimeout = 600,
    silent = FALSE, verbose = FALSE)
}
\arguments{
  \item{inputs,}{optional. List of inputs as a vector of
  character, each containing valid paths to Manta objects
  that are the intended job input files. You may use output
  from mantaFind() or mantaLs.paths() here. If you have no
  inputs, your initial Job task must be a mantaReduce()
  step.}

  \item{job,}{required. The R job structure as created with
  mantaJob.Setup() and Map and Reduce job tasks as defined
  therein by one or more mantaMap() and/or mantaReduce()
  steps. More information and parameters are explained in
  the help for these three functions.}

  \item{batchsize}{numeric. Maximum number of input object
  paths to upload in one batch to the running job. Default
  is 500.}

  \item{watch}{logical. Call mantaJob.done() in polling
  mode, sleeping for for sleep seconds up to the duration
  of the watchtimeout value in seconds. This causes the
  function to wait until the job is done to return, or
  timed out. Timeout does not imply job success or
  failure.}

  \item{sleep}{integer. Number of seconds to wait between
  status requests in polling mode when watch is set to
  TRUE. Default is 30 seconds.}

  \item{watchtimeout}{integer. Number of seconds after
  which polling ends. Passed to mantaJob.done() when watch
  is set to TRUE. Default is 10 minutes (600 seconds).. If
  watchtimeout is exceeded, it simply means the job is
  still running or queued on Manta. mantaJob.done() or
  mantaJob.status() can be called after.}

  \item{silent}{logical. Supress console messages, does not
  affect verbose setting.}

  \item{verbose}{logical optional. Passed to RCURL to
  reveal HTTP communication.}
}
\value{
  Returns a Manta Status JSON structure. The Manta Job
  identifier is the "id": field - like this
  "70c30bab-873b-66da-ebc8-ced12bd35ac4". This value is the
  jobid parameter to be used used by other mantaJob
  functions for information, error and output retrieval as
  a lookup key. This key can also be used by Node.js Manta
  command-line mjob commands.
}
\description{
  Job is created by mantaJob.setup() and tasks as defined
  therein by mantaMap(), and/or mantaReduce() functions.
  Note that Manta tasks are UNIX shell commands, not native
  R commands.
}
\details{
  Example - Manta Map/Reduce UNIX Word Count:

  status <- mantaJob.launch( inputs =
  mantaLs.paths("~~/public/shakespeare", grepfor =
  "[.]txt"), job = mantaJob.setup( name = "word count",
  mantaMap("wc"), mantaReduce("awk '\{ l += $1; w += $2; c
  += $3 \} END \{ print l, w, c \}'") ) )

  Getting Job Results: mantaJob.status(jobid) # check to
  see if job is complete, as JSON information
  mantajob.done(jobid) # returns logical job done
  (TRUE/FALSE) mantaJob.outputs(jobid) # retrieve list of
  paths to Manta output objects mantaJob.errors(jobid) #
  retrieve JSON formatted job error information
}
\keyword{Manta,}
\keyword{manta}

