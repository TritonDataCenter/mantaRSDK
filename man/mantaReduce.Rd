\name{mantaReduce}
\alias{mantaReduce}
\title{Constructor for R format Manta Job for Reduce Unix task.}
\usage{
  mantaReduce(exec, init, assets, reducers, memory, disk)
}
\arguments{
  \item{reducers}{integer. Number of reducers to use from 1
  to 1024. Use with caution.}

  \item{exec}{character required. The Unix shell command to
  be executed in the Map task operating on the input Manta
  objects specified when the job is launched. \code{exec}
  may be any valid Unix shell command capable of running on
  the Manta compute node at execution time. Use the Node.js
  command \code{mlogin} to test out commands. Pipelines and
  shell syntax escaping and substitution are all supported.
  You can also execute your own programs stored as Manta
  objects by including them with the \code{assets}
  parameter and referencing them from the exec command from
  the \code{/assets} folder. See
  \code{http://apidocs.joyent.com/manta/jobs-reference.html}
  for more details.}

  \item{init}{character optional. A Unix shell command
  executed prior to the \code{exec} command. Used to run
  initialization steps on the Manta compute node prior to
  task execution. \code{init} can also execute programs
  stored as Manta objects mounted as POSIX read-only files
  mounted at \code{/assets}. For example it can unpack a
  \code{tar} \code{asset} before running \code{exec}.}

  \item{assets}{array of character, optional. Specify Manta
  objects that are to be accessed by the compute node at
  job runtime. Include shell scripts, installation steps
  configuration steps, custom executables compiled for
  SmartOS, or \code{tar} files as you require here. At job
  runtime, each node will provide the specified Manta
  objects as POSIX files at the \code{/assets} directory
  for read-only access from your \code{exec} or \code{init}
  shell commands. For example a Manta object listed as an
  asset that lives at \code{ ~~/stor/data.tgz} will be
  found by your script on the Manta compute node as a
  mounted read-only POSIX file at
  \code{/assets/~~/stor/data.tgz} where \code{~~} is your
  Manta username.}

  \item{memory}{integer optional. Amount of memory
  requested for Manta compute node instance. \code{128,
  256, 512, 1024, 2048, 8192,} or \code{16384} are valid
  values in MB. Default is \code{1024} MB.}

  \item{disk}{integer optional. Amount of temporary working
  disk (not Manta storage space) to be used by the compute
  node when executing the task. Valid values are: \code{2,
  4, 8, 16, 32, 64, 128, 256, 512,} or \code{1024} GB.
  Default is \code{8} GB. Writeable disk on each compute
  node is found at the \code{/var/tmp} directory during
  \code{init} or \code{exec} job runtime. To save data from
  this space onto permanent Manta storage, use the Node.js
  command \code{mput} in your exec script to upload the
  files from \code{/var/tmp} onto Manta storage.}
}
\value{
  Returns an R list for consumption by
  \code{mantaJob.setup}
}
\description{
  Helper function to construct R structure describing a
  Reduce task. To be used to satisfy the \code{...}
  argument of \code{mantaJob.setup} and specify the Unix
  command line task, any initialization tasks, an array of
  Manta filesystem \code{asset} files, and the memory/disk
  size to be used for the compute instance on Manta.
}
\details{
  On Manta, a Reduce task phase executes a generic UNIX
  command when specified in \code{mantaJob.launch}, or from
  the output pipelined from a previous step. Use
  \code{mantaReduce} to run a job that has no Manta object
  input data.

  The \code{exec} argument must be a valid generic UNIX
  command line, not an R function. The \code{exec} argument
  may call executables or runtime language scripts that are
  hosted on Manta and specified as \code{assets}.  The
  \code{init} parameter command is called before the
  \code{exec} argument and is not passed input. The init
  argument may be used, for example to extract scripts from
  an \code{asset} on Manta saved as a \code{tar} object.

  Note that you do not have to specify the input for a
  Reduce task for \code{mantaJob.launch}, the service pipes
  the output of the previous Map task phase as input to the
  Reduce task. Note also tha the piped input for a Reduce
  task may arrive in any order, no sorting is done by the
  service to the pipe between Map and Reduce tasks.
}
\examples{
\dontrun{
# Example - Map/Reduce Unix Word Count
job <- mantaJob.setup("word count",
         mantaMap("wc"),
         mantaReduce("awk '{ l += $1; w += $2; c += $3 } END { print l, w, c }'"))
}
}
\seealso{
  Other mantaJobs: \code{\link{mantaJob.cancel}},
  \code{\link{mantaJob.done}},
  \code{\link{mantaJob.errors}},
  \code{\link{mantaJob.errors.stderr}},
  \code{\link{mantaJob.failures}},
  \code{\link{mantaJob.inputs}},
  \code{\link{mantaJob.launch}},
  \code{\link{mantaJob.outputs}},
  \code{\link{mantaJob.outputs.cat}},
  \code{\link{mantaJob.setup}},
  \code{\link{mantaJob.status}}, \code{\link{mantaJobs}},
  \code{\link{mantaJobs.running}},
  \code{\link{mantaJobs.tail}}, \code{\link{mantaMap}}
}

