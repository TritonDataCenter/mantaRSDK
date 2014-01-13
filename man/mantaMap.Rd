\name{mantaMap}
\alias{mantaMap}
\title{Constructor for R format Manta Job for Map Unix task.}
\usage{
  mantaMap(exec, init, assets, memory, disk)
}
\arguments{
  \item{exec}{character required. The Unix shell command to
  be executed in the Map task operating on the input Manta
  objects specified when the job is launched. exec may be
  any valid Unix shell command capable of running on the
  Manta compute node at execution time. Use the Node.js
  command mlogin to test out commands. Pipelines and shell
  syntax escaping and substitution are all supported. You
  can also execute programs stored in the service by
  including them with the assets parameter and referencing
  them from the exec command from /assets folder. See
  http://apidocs.joyent.com/manta/jobs-reference.html for
  more details.}

  \item{init}{character optional. A Unix shell command
  executed prior to the exec command. Used to run
  initialization steps on the Manta compute node prior to
  task execution. init can also execute programs stored as
  Manta objects mounted as POSIX read-only files mounted at
  /assets.}

  \item{assets}{array of character, optional. Your files
  stored as Manta objects that are to be accessed by the
  compute node at job runtime. Include any shell scripts,
  installation steps configuration steps, custom
  executables compiled for SmartOS, or tar files you
  require here. At job runtime, each node will provide the
  specified Manta objects as POSIX files at /assets for
  read-only access from your exec or init shell commands.
  For example a Manta object at ~~/stor/data.tgz will be
  found by your script as a mounted read-only POSIX file at
  /assets/~~/stor/data.tgz (~~ is your Manta username).}

  \item{memory}{optional. Amount of memory requested for
  Manta compute node instance. 128, 256, 512, 1024, 2048,
  8192, or 16384 are valid values in MB. Default is 1024
  GB.}

  \item{disk}{optional. Amount of temporary working disk
  (not Manta storage space) to be used by the compute node
  when executing the task. Valid values are 2, 4, 8, 16,
  32, 64, 128, 256, 512, or 1024 GB. Default is 8GB.
  Writeable disk on each compute node is found at the
  /var/tmp directory during init or exec job runtime. To
  save data from this space onto permanent Manta storage,
  use the Node.js command mput in your exec script to
  upload the files from /var/tmp onto Manta storage.}
}
\value{
  Returns an R list describing a Map task phase for
  consumption by mantaJob.setup()
}
\description{
  Helper function to construct R structure describing a Map
  task. To be used to satisfy the ... argument of
  mantaJob.setup() and specify the Unix command line task,
  any initialization tasks, an array of Manta filesystem
  asset files, and the memory/disk size to be used for the
  compute instance on Manta. On Manta, a Map task phase
  executes a generic UNIX command given some input Manta
  object list which is specified in mantaJob.launch(),
  which distributes the jobs to compute instances local to
  the manta object location. The exec argument must be a
  valid generic UNIX command line, not an R function. The
  exec argument may call executables or runtime language
  scripts that are hosted on Manta and specified as assets.
  The init argument is called before the exec argument and
  is not passed input. The init argument may be used, for
  example to extract scripts from an asset on Manta saved
  as a tar file,
}
\details{
  Example - Map/Reduce Unix Word Count

  job <- mantaJob.setup("word count", mantaMap("wc"),
  mantaReduce("awk '\{ l += $1; w += $2; c += $3 \} END \{
  print l, w, c \}'"))

  To launch the job, specify the inputs and job R structure
  made with mantaJob.setup like so:

  mantaJob.launch(mantaLs.paths("~~/public/shakespeare",
  grepfor = "[.]txt"), job)
}
\keyword{Manta,}
\keyword{manta}

