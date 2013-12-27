\name{mantaJob.setup}
\alias{mantaJob.setup}
\title{Constructor for R format Manta Job including name, and tasks as defined by
mantaMap(), and/or mantaReduce() functions. Does not specify job input.}
\usage{
  mantaJob.setup(name, ...)
}
\arguments{
  \item{name}{character, optional.}

  \item{...}{One or more mantaMap() mantaReduce() functions
  with arguments in order of task execution}
}
\description{
  Function to construct R structure for mantaJob.launch.
  Specify a name for the Manta job and tasks to execute via
  one or more calls to mantaMap() and/or mantaReduce()
  which define and parameterize each task. mantaJob.setup
  combines tasks into a job pipeline structure for
  mantaJob.launch See mantaMap() and mantaReduce() for
  details of their arguments. The exec argument must be a
  valid generic UNIX command line, not an R function.
}
\details{
  Example - Map/Reduce Unix Word Count

  job <- mantaJob.setup("word count", mantaMap("wc"),
  mantaReduce("awk '{ l += $1; w += $2; c += $3 } END {
  print l, w, c }'"))

  mantaJob.launch(inputs =
  mantaLs.paths("~~/public/shakespeare", grepfor =
  "[.]txt"), job)
}
\keyword{Manta,}
\keyword{manta}

