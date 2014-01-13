# Roxygen Comments mantaJob.errors
#' Returns JSON Manta error messages given Manta job identifier.
#'
#' @param jobid character optional. Manta job identifier such as
#' "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default uses mantaJobs.tail()
#' to fetch the jobid of the last Manta Job run on the service
#'
#' @param readable logical. Set to FALSE to return the JSON error strings, or
#' NULL if no errors found..
#' Default TRUE pretty prints JSON to the console.
#'
#' @keywords Manta, manta
#'
#'
#' JSON error message return values:\cr
#' Name		Type	Description\cr
#' ------------------------
#' id:  String. Job id\cr
#' phase: Number. Phase number of the failure\cr
#' what: String. A human readable summary of what failed\cr
#' code: String. Programmatic error code\cr
#' message: String. Human readable error message\cr
#' stderr: String (optional). A key that saved the stderr for the given command\cr
#' key:	String (optional). The input key being processed when
#' the task failed (if the service can determine it)\cr
#'
#' @export
mantaJob.errors <-
function(jobid, readable = TRUE) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  if (missing(jobid)) {
    jobid <- mantaJobs.tail()
  }
  ## Look for live/err
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/err", sep="")
  result <-  mantaAttempt(action, method = "HEAD", returncode = 200,  silent = TRUE, test = TRUE)
  buffer <- FALSE
  if (result == FALSE) {
    ## Look for archived
    action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/err.txt", sep="")
    buffer <-  mantaGet(action, buffer = TRUE, info = FALSE)
  } else {
    buffer <-  mantaGet(action, buffer = TRUE, info = FALSE)
  }
  if (length(buffer) != 1) {
    jsonerrs <- rawToChar(buffer)
  } else {
     if (readable == TRUE) {
       cat(paste("Job errors for ", jobid, " not found.\n", sep = ""))
     } 
     return(NULL)
  }

  jsonlines <- strsplit(jsonerrs, split = "\n")
 
  prettyoutput <- function(line) {
     cat(paste(toJSON(fromJSON(line), pretty=TRUE), "\n", sep=""))
  }

  if (readable == TRUE) {
    lapply(jsonlines[[1]], prettyoutput) 
  } else {
    return(jsonlines[[1]])
  }
 cat("\n")
} 
