# Roxygen Comments mantaJob.status
#' Returns JSON Manta job status data given Manta job identifier.
#'    
#' @inheritParams mantaJob.outputs
#'
#' @param readable logical. Set to \code{FALSE} to return the JSON Job as character(), or
#' \code{NULL} if no Job status found.
#' Default \code{TRUE} pretty prints JSON Job status to the console.
#'
#' @keywords Manta, manta
#'
#' @family mantaJobs
#'
#' @examples
#' \dontrun{
#' ## Retrieve JSON status of the last run Manta job:
#' mantaJob.status(readable = FALSE) -> status
#' ## Show JSON status of last run Manta job:
#' mantaJob.status()
#' }
#'
#' @export
mantaJob.status <-
function(jobid, readable = TRUE) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  if (missing(jobid)) {
    jobid <- mantaJobs.tail()
  }
  ## Look for live/err
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/status", sep="")
  result <-  mantaAttempt(action, method = "GET", returncode = 204,  json = TRUE, silent = TRUE, test = TRUE)
  if (result == FALSE) {
    ## Look for archived
    action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/job.json", sep="")
    json <-  mantaAttempt(action, method = "GET", returncode = 200,  json = TRUE, silent = TRUE)
  } else {
    json <- mantaAttempt(action, method = "GET", returncode = 204,  json = TRUE, silent = TRUE)
  }
  if(json$lines[1] != "") {
     if (readable == TRUE) {
       cat(paste(toJSON(fromJSON(json$lines), pretty=TRUE), "\n", sep=""))
     } else {
       return(json$lines)
    }
  } else {
     if (readable == TRUE) {
      cat(paste("Job status for ", jobid, " not found.\n", sep = ""))
    } else {
      return(NULL)
    }
  }
}
