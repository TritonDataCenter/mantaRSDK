#' mantaRSDK
#'
#' Joyent Manta Storage Service R Software Development Kit
#'  
#'  R functions to transmit/receive native R data and 
#'  files to the Manta Storage Service for object storage..
#' 
#'  Manta jobs can compute on stored objects with Map/Reduce and
#'  UNIX shell commands in the cloud. Core functions communicate via
#'  the Manta REST API using RCurl. OpenSSL is required for
#'  authentication support, and must be installed separate from R. 
#'
#'  To use this library you require a Joyent Manta account
#'  http://www.joyent.com
#'  
#'  Configuration requires 3 environment variables
#'  MANTA_USER
#'  MANTA_KEY
#'  MANTA_URL
#'  and your private SSL key as registered with Joyent 
#'
#'
#'
#' Manta Account Management 
#'
#'   mantaAccount()
#'   mantaWhoami()
#'   mantaGetLimits()
#'   mantaSetLimits()
#'
#' Manta Hierarchical Directory Operations
#'
#'   mantaGetwd()
#'   mantaSetwd()
#'   mantaSetwd.jobs()
#'   mantaSetwd.public()
#'   mantaSetwd.reports()
#'   mantaSetwd.stor()
#'   mantaSetwd.ws()
#'   mantaMkdir()
#'   mantaRmdir()
#'   mantaLs()
#'   mantaLs.du()
#'   mantaLs.l()
#'   mantaLs.n()
#'   mantaLs.paths()
#'   mantaLs.url()
#'   mantaFind()
#'   mantaFind.du()
#'   mantaFind.l()
#'   mantaFind.n()
#'   mantaFind.sizepath()
#'   mantaFind.sizes()
#'   mantaFind.url()
#'
#' Manta Object Store Operations
#'
#'   mantaExists()
#'   mantaPut()
#'   mantaGet()
#'   mantaCat()
#'   mantaRm()
#'   mantaSnapln()
#'   mantaDump()
#'   mantaSource()
#'   mantaSave()
#'   mantaLoad()
#'   mantaSave.ws()
#'   mantaLoad.ws()
#'
#' Manta Compute Job Operations
#'
#'   mantaJob.setup()
#'   mantaMap()
#'   mantaReduce()
#'   mantaJob.launch()
#'   mantaJob.status()
#'   mantaJob.done()
#'   mantaJob.cancel()
#'   mantaJob.errors()
#'   mantaJob.errors.stderr()
#'   mantaJob.failures()
#'   mantaJob.inputs()
#'   mantaJob.outputs()
#'   mantaJob.outputs.cat()
#'   mantaJobs()
#'   mantaJobs.running()
#'   mantaJobs.tail()
#'
#' Exposed Low Level Calls
#'
#'   mantaAttempt()
#'   mantaXfer()
#'   mantaSave.image()
#'
#' Useful Bunyan Debug/Log Utilities
#' 
#'   bunyanSetLog()
#'   bunyanBuffer()
#'   bunyanTraceback()
#'
#' ------DETAILS--------
#'
#' Manta Account Management 
#'
#'   mantaAccount()\cr
#'   Changes current Manta account information.
#'
#'   mantaWhoami()\cr
#'   Report the active Manta account settings.
#'
#'   mantaGetLimits()\cr
#'   Returns Manta durability level, connection timeouts and limits currently active.
#'
#'   mantaSetLimits()\cr
#'   Sets Manta durability level, connection timeouts and limits currently active.
#' 
#' Manta Hierarchical Directory Operations
#'
#'   mantaGetwd()\cr
#'   Gets Manta working directory.
#'
#'   mantaSetwd()\cr
#'   mantaSetwd.public()\cr
#'   mantaSetwd.stor()\cr
#'   mantaSetwd.ws()\cr
#'   mantaSetwd.jobs()\cr
#'   mantaSetwd.reports()\cr
#'   Sets Manta working directory. Dotted forms are top-level (public, stor, jobs, reports) or
#'   workspace (as set by mantaSave.ws).
#'
#'   mantaMkdir()\cr
#'   Makes a Manta subdirectory, optionally with parent directories.
#'
#'   mantaRmdir()\cr
#'   Removes a Manta subdirectory.  
#'
#'   mantaLs()\cr
#'   mantaLs.du()\cr
#'   mantaLs.l()\cr
#'   mantaLs.n()\cr
#'   mantaLs.paths()\cr
#'   mantaLs.url()\cr
#'   Lists, searches, filters, sorts and formats Manta directory listings. Dotted forms alter
#'   the format of the output. Numerical values are returned by n (number) and du (disk used).
#'   
#'
#'   mantaFind()\cr
#'   mantaFind.du()\cr
#'   mantaFind.l()\cr
#'   mantaFind.n()\cr
#'   mantaFind.sizepath()\cr
#'   mantaFind.sizes()\cr
#'   mantaFind.url()\cr
#'   Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.
#'   Dotted forms alter the format of the output. Numerical values are returned by n (number)
#'   and du (disk used).
#'
#' Manta Object Storage Operations
#'
#'   mantaExists()\cr
#'   Tests to see if a Manta object or subdirectory exists.
#'
#'   mantaPut()\cr
#'   Uploads file(s) (vectorized), or raw R buffer data to Manta Storage Service.
#'
#'   mantaGet()\cr
#'   Downloads Manta object(s) (vectorized) specified to file(s) or buffer.
#'
#'   mantaCat()\cr
#'   Retrieves object from Manta and uses cat() to print contents to the R console.
#'
#'   mantaRm()\cr
#'   Removes specified Manta object, optionally recursive.
#'
#'   mantaSnapln()\cr
#'   Makes a Snaplink - combination snapshot and symbolic link.
#'
#'   mantaDump()\cr
#'   Uses dump() to upload text parsable R data to Manta Storage Service.
#'
#'   mantaSource()\cr
#'   Downloads specified Manta object and applies source() to parse R code file.
#'
#'   mantaSave()\cr
#'   Uploads R data to Manta Storage Service using R function save().
#'
#'   mantaLoad()\cr
#'   Downloads specified Manta object containing R data and uses R function load().
#'
#'   mantaSave.ws()\cr
#'   Saves R workspace to Manta R workspace directory with an audit trail of backups.
#'
#'   mantaLoad.ws()\cr
#'   Loads last current R workspace from Manta R workspace directory.
#'
#'
#' Compute Job Operations
#'
#'   mantaJob.setup()\cr
#'   mantaMap()\cr
#'   mantaReduce()\cr
#'   Constructors for R format Manta Job including name, and UNIX command line tasks as defined by
#'   mantaMap(), and/or mantaReduce() functions. 
#'
#'   mantaJob.launch()\cr
#'   Submits list of input Manta objects and R format Manta Job specification, runs job
#'   optionally polls job status. Returns job status.
#'
#'   mantaJob.cancel()\cr
#'   Sends Manta a cancel message to stop running job.
#'
#'   mantaJob.status()\cr
#'   Returns JSON Manta job status data given Manta job identifier.
#'
#'   mantaJob.done()\cr
#'   Checks or polls status of a Manta job. Returns done or not as logical.
#'
#'   mantaJob.errors()\cr
#'   Returns JSON Manta error messages given Manta job identifier.
#'
#'   mantaJob.errors.stderr()\cr
#'   Retrieves JSON errors given Manta job identifier, then retrieves each stderr message
#'   archived on Manta (if any) and uses mantaCat() to print contents of stderr 
#'   to the console.
#'
#'   mantaJob.failures()\cr
#'   Returns list of failures given Manta job identifier.
#'
#'   mantaJob.inputs()\cr
#'   Returns list of input Manta objects given Manta job identifier.
#'
#'   mantaJob.outputs()\cr
#'   Returns list of output Manta objects given Manta job identifier.
#' 
#'   mantaJob.outputs.cat()\cr
#'   Retrieves list of Manta output objects given Manta job identifier, then
#'   retrieves each object from Manta and uses cat() to print contents
#'   to the R console.
#'
#'   mantaJobs()\cr
#'   Lists all Manta job identifiers, sorted by time.
#'
#'   mantaJobs.running()\cr
#'   Lists identifiers of any running Manta jobs.
#'
#'   mantaJobs.tail()\cr
#'   Returns identifier of last run Manta job identifier, or from offset n up from end of list.
#'
#' Exposed Low Level Calls
#'
#'   mantaAttempt()\cr
#'   raw REST API Manta Caller with exception handling, used by many functions.
#'
#'   mantaXfer()\cr
#'   raw REST API Manta Caller for mantaPut() mantaGet() and related data transfer routines.
#'
#'   mantaSave.image()\cr
#'   Workspace Upload function that calls R save.image(); used by mantaSave.ws().
#'
#' Useful Bunyan Debug/Log Utilities
#' 
#'   bunyanSetLog()\cr
#'   Starts bunyan JSON message logging at  supplied logging threshold to file or memory buffer.
#'
#'   bunyanBuffer()\cr
#'   Returns memory buffer.
#'
#'   bunyanTraceback()\cr
#'   Get messages from memory after last bunyanSetpoint
#'
#' 
#' @references http://apidocs.joyent.com/manta/
#' @import PKI digest RCurl RJSONIO 
#' @name mantaRSDK
#' @docType package
NULL
