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
#' Account Management 
#'
#'   mantaAccount()
#'   mantaWhoami()
#'   mantaGetLimits()
#'   mantaSetLimits()
#'
#' Directory Operations
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
#' Object Operations
#'
#'   mantaExists()
#'   mantaMetadata()
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
#' Compute Job Operations
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
#'   bunyanSetpoint()
#'   bunyanClearSetpoint()
#'   bunyanTraceback()
#'   bunyanTracebackN()
#'   bunyanTracebackErrors()
#'   bunyanTracebackWarnings()
#'
#' 
#' @references http://apidocs.joyent.com/manta/
#' @import PKI digest RCurl RJSONIO 
#' @name mantaRSDK
#' @docType package
NULL
