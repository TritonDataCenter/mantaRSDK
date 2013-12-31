# Roxygen Comments mantaJob.errors
#' mantaJob.errors returns JSON error messages given Manta job identifier
#' 
#' JSON error message return values
#' Name		Type	Description
#' ------------------------
#' id		String	job id
#' phase	Number	phase number of the failure
#' what		String	a human readable summary of what failed
#' code		String	programmatic error code
#' message	String	human readable error message
#' stderr	String	(optional) a key that saved the stderr for the given command
#' key		String	(optional) the input key being processed when 
#' 			the task failed (if the service can determine it)
#'
#'
#'
#'
#' @param jobid character required. Manta job identifier such as
#' "70c30bab-873b-66da-ebc8-ced12bd35ac4"  or use mantaJob.last()
#' to fetch the jobid of the last manta Job run on the service
#' e.g. mantaJob.errors(mantaJob.last())
#' 
#'
#' @param readable logical. Set to FALSE to return the JSON error strings, or
#' NULL if no errors found..
#' Default TRUE pretty prints JSON to the console.
#'
#' @keywords Manta, manta
#'
#' @export
mantaJob.errors <-
function(jobid, readable = TRUE) {
  if (missing(jobid)) stop("No job identifier provided")
  ## Look for live/err
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/err", sep="")
  result <-  mantaAttempt(action, method = "GET", returncode = 200,  json = TRUE, silent = TRUE, test = TRUE)
  if (result == FALSE) {
    ## Look for archived
    action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/err.txt", sep="")
    json <-  mantaAttempt(action, method = "GET", returncode = 200,  json = TRUE, silent = TRUE)
  } else {
    json <- mantaAttempt(action, method = "GET", returncode = 200,  json = TRUE, silent = TRUE)
  }
  if(json$lines != "") { 
     if (readable == TRUE) {
       cat(paste(toJSON(fromJSON(json$lines), pretty=TRUE), "\n", sep=""))
     } else {
       return(json$lines)
    }
  } else {
     if (readable == TRUE) {
      cat(paste("Job errors for ", jobid, " not found.\n", sep = ""))
    } else {
      return(NULL)
    }
  }
}
