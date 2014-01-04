# Roxygen Comments mantaJob.status
#' Returns JSON Manta job status data given Manta job identifier
#'
#'#' @param jobid character optional. Manta job identifier such as
#' "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default uses mantaJobs.tail()
#' to fetch the jobid of the last Manta Job run on the service
#'
#' @param readable logical. Set to FALSE to return the JSON Job as character(), or
#' NULL if no Job status found..
#' Default TRUE pretty prints JSON Job status to the console.
#'    
#' @keywords Manta, manta
#'
#' @export
mantaJob.status <-
function(jobid, readable = TRUE) {
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
