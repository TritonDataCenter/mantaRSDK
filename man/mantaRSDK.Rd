\docType{package}
\name{mantaRSDK}
\alias{mantaRSDK}
\alias{mantaRSDK-package}
\title{mantaRSDK}
\description{
  R functions to transmit/receive native R data and files
  to the Manta Storage Service for object storage.

  Manta jobs can compute on stored objects with Map/Reduce
  and UNIX shell commands in the cloud. Core functions
  communicate via the Manta REST API using RCurl.  OpenSSL
  is required for authentication support, and must be
  installed separate from R.

  To use this library you require a Joyent Manta account
  http://www.joyent.com

  Configuration requires 3 environment variables
  \code{$MANTA_USER}, \code{$MANTA_KEY}, and
  \code{$MANTA_URL} and your SSH keys as registered with
  Joyent.

  FOR COMPLETE INSTALLATION INSTRUCTIONS - SEE:\cr
  \code{https://github.com/cwvhogue/mantaRSDK/blob/master/README.md}
}
\details{
  Joyent Manta Storage Service R Software Development Kit
}
\section{RSDK Functions}{
  Manta Account Management

  \code{\link{mantaAccount}} \code{\link{mantaWhoami}}
  \code{\link{mantaGetLimits}} \code{\link{mantaSetLimits}}

  Manta Hierarchical Directory Operations

  \code{\link{mantaGetwd}} \code{\link{mantaSetwd}}
  \code{\link{mantaSetwd.jobs}}
  \code{\link{mantaSetwd.public}}
  \code{\link{mantaSetwd.reports}}
  \code{\link{mantaSetwd.stor}} \code{\link{mantaSetwd.ws}}
  \code{\link{mantaMkdir}} \code{\link{mantaRmdir}}
  \code{\link{mantaLs}} \code{\link{mantaLs.du}}
  \code{\link{mantaLs.l}} \code{\link{mantaLs.n}}
  \code{\link{mantaLs.paths}} \code{\link{mantaLs.url}}
  \code{\link{mantaFind}} \code{\link{mantaFind.du}}
  \code{\link{mantaFind.l}} \code{\link{mantaFind.n}}
  \code{\link{mantaFind.sizepath}}
  \code{\link{mantaFind.sizes}} \code{\link{mantaFind.url}}

  Manta Object Store Operations

  \code{\link{mantaExists}} \code{\link{mantaPut}}
  \code{\link{mantaGet}} \code{\link{mantaCat}}
  \code{\link{mantaRm}} \code{\link{mantaSnapln}}
  \code{\link{mantaDump}} \code{\link{mantaSource}}
  \code{\link{mantaSave}} \code{\link{mantaLoad}}
  \code{\link{mantaSave.ws}} \code{\link{mantaLoad.ws}}

  Manta Compute Job Operations

  \code{\link{mantaJob.setup}} \code{\link{mantaMap}}
  \code{\link{mantaReduce}} \code{\link{mantaJob.launch}}
  \code{\link{mantaJob.status}} \code{\link{mantaJob.done}}
  \code{\link{mantaJob.cancel}}
  \code{\link{mantaJob.errors}}
  \code{\link{mantaJob.errors.stderr}}
  \code{\link{mantaJob.failures}}
  \code{\link{mantaJob.inputs}}
  \code{\link{mantaJob.outputs}}
  \code{\link{mantaJob.outputs.cat}}
  \code{\link{mantaJobs}} \code{\link{mantaJobs.running}}
  \code{\link{mantaJobs.tail}}

  Exposed Low Level Calls

  \code{\link{mantaAttempt}} \code{\link{mantaXfer}}
  \code{\link{mantaSave.image}}

  Useful Bunyan Debug/Log Utilities

  \code{\link{bunyanSetLog}} \code{\link{bunyanBuffer}}
  \code{\link{bunyanTraceback}}
}

\section{Details}{
  Manta Account Management

  \code{\link{mantaAccount}}\cr Changes current Manta
  account information.

  \code{\link{mantaWhoami}}\cr Report the active Manta
  account settings.

  \code{\link{mantaGetLimits}}\cr Returns Manta durability
  level, connection timeouts and limits currently active.

  \code{\link{mantaSetLimits}}\cr Sets Manta durability
  level, connection timeouts and limits currently active.

  Manta Hierarchical Directory Operations

  \code{\link{mantaGetwd}}\cr Gets Manta working directory.

  \code{\link{mantaSetwd}}\cr
  \code{\link{mantaSetwd.public}}\cr
  \code{\link{mantaSetwd.stor}}\cr
  \code{\link{mantaSetwd.ws}}\cr
  \code{\link{mantaSetwd.jobs}}\cr
  \code{\link{mantaSetwd.reports}}\cr Sets Manta working
  directory. Dotted forms are top-level (public, stor,
  jobs, reports) or workspace (as set by
  \code{\link{mantaSave.ws}}).

  \code{\link{mantaMkdir}}\cr Makes a Manta subdirectory,
  optionally with parent directories.

  \code{\link{mantaRmdir}}\cr Removes a Manta subdirectory.

  \code{\link{mantaLs}}\cr \code{\link{mantaLs.du}}\cr
  \code{\link{mantaLs.l}}\cr \code{\link{mantaLs.n}}\cr
  \code{\link{mantaLs.paths}}\cr
  \code{\link{mantaLs.url}}\cr Lists, searches, filters,
  sorts and formats Manta directory listings. Dotted forms
  alter the format of the output. Numerical values are
  returned by n (number) and du (disk used).

  \code{\link{mantaFind}}\cr \code{\link{mantaFind.du}}\cr
  \code{\link{mantaFind.l}}\cr \code{\link{mantaFind.n}}\cr
  \code{\link{mantaFind.sizepath}}\cr
  \code{\link{mantaFind.sizes}}\cr
  \code{\link{mantaFind.url}}\cr Recursive find tool for
  retrieving matching objects/subdirs from Manta hierarchy.
  Dotted forms alter the format of the output. Numerical
  values are returned by n (number) and du (disk used).

  Manta Object Storage Operations

  \code{\link{mantaExists}}\cr Tests to see if a Manta
  object or subdirectory exists.

  \code{\link{mantaPut}}\cr Uploads file(s) (vectorized),
  or raw R buffer data to Manta Storage Service.

  \code{\link{mantaGet}}\cr Downloads Manta object(s)
  (vectorized) specified to file(s) or buffer.

  \code{\link{mantaCat}}\cr Retrieves object from Manta and
  uses \code{cat} to print contents to the R console.

  \code{\link{mantaRm}}\cr Removes specified Manta object,
  optionally recursive.

  \code{\link{mantaSnapln}}\cr Makes a Snaplink -
  combination snapshot and symbolic link.

  \code{\link{mantaDump}}\cr Uses \code{dump} to upload
  text parsable R data to Manta Storage Service.

  \code{\link{mantaSource}}\cr Downloads specified Manta
  object and applies \code{source} to parse R code file.

  \code{\link{mantaSave}}\cr Uploads R data to Manta
  Storage Service using R function \code{save}.

  \code{\link{mantaLoad}}\cr Downloads specified Manta
  object containing R data and uses R function \code{load}.

  \code{\link{mantaSave.ws}}\cr Saves R workspace to Manta
  R workspace directory with an audit trail of backups.

  \code{\link{mantaLoad.ws}}\cr Loads last current R
  workspace from Manta R workspace directory.

  Compute Job Operations

  \code{\link{mantaJob.setup}}\cr \code{\link{mantaMap}}\cr
  \code{\link{mantaReduce}}\cr Constructors for R format
  Manta Job including name, and UNIX command line tasks as
  defined by \code{\link{mantaMap}}, and/or
  \code{\link{mantaReduce}} functions.

  \code{\link{mantaJob.launch}}\cr Submits list of input
  Manta objects and R format Manta Job specification, runs
  job optionally polls job status. Returns job status.

  \code{\link{mantaJob.cancel}}\cr Sends Manta a cancel
  message to stop running job.

  \code{\link{mantaJob.status}}\cr Returns JSON Manta job
  status data given Manta job identifier.

  \code{\link{mantaJob.done}}\cr Checks or polls status of
  a Manta job. Returns done or not as logical.

  \code{\link{mantaJob.errors}}\cr Returns JSON Manta error
  messages given Manta job identifier.

  \code{\link{mantaJob.errors.stderr}}\cr Retrieves JSON
  errors given Manta job identifier, then retrieves each
  stderr message archived on Manta (if any) and uses
  \code{\link{mantaCat}} to print contents of stderr to the
  console.

  \code{\link{mantaJob.failures}}\cr Returns list of
  failures given Manta job identifier.

  \code{\link{mantaJob.inputs}}\cr Returns list of input
  Manta objects given Manta job identifier.

  \code{\link{mantaJob.outputs}}\cr Returns list of output
  Manta objects given Manta job identifier.

  \code{\link{mantaJob.outputs.cat}}\cr Retrieves list of
  Manta output objects given Manta job identifier, then
  retrieves each object from Manta and uses \code{cat} to
  print contents to the R console.

  \code{\link{mantaJobs}}\cr Lists all Manta job
  identifiers, sorted by time.

  \code{\link{mantaJobs.running}}\cr Lists identifiers of
  any running Manta jobs.

  \code{\link{mantaJobs.tail}}\cr Returns identifier of
  last run Manta job identifier, or from offset n up from
  end of list.

  Exposed Low Level Calls

  \code{\link{mantaAttempt}}\cr raw REST API Manta Caller
  with exception handling, used by many functions.

  \code{\link{mantaXfer}}\cr raw REST API Manta Caller for
  \code{\link{mantaPut}} \code{\link{mantaGet}} and related
  data transfer routines.

  \code{\link{mantaSave.image}}\cr Workspace Upload
  function that calls R \code{save.image}; used by
  \code{\link{mantaSave.ws}}.

  Useful Bunyan Debug/Log Utilities

  \code{\link{bunyanSetLog}}\cr Starts bunyan JSON message
  logging at supplied logging threshold to file or memory
  buffer.

  \code{\link{bunyanBuffer}}\cr Returns memory buffer.

  \code{\link{bunyanTraceback}}\cr Get messages from memory
  after last bunyanSetpoint
}
\references{
  http://apidocs.joyent.com/manta/
}

