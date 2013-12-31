# Roxygen Comments mantaJob.output
#' Returns list of output objects given Manta job identifier
#'
#'
#' @param jobid character required. Manta job identifier such as
#' "70c30bab-873b-66da-ebc8-ced12bd35ac4"  or use mantaJob.last()
#' to fetch the jobid of the last manta Job run on the service
#' e.g. mantaJob.output(mantaJob.last())
#'    
#' @param silent logical required. Set to TRUE for non-interactive
#' use of the function to suppress stop() on Manta Service error messages,
#' and return an empty data set. N.B. Errors are logged and in the bunyan
#' buffer. 
#'
#'
#' @keywords Manta, manta
#'
#' @export
mantaJob.output <-
function(jobid, silent = FALSE) {
  if (missing(jobid)) stop("No job identifier provided")
  ## Look for live/err
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/out", sep="")
  result <-  mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE, silent = TRUE, test = TRUE)
  if (result == FALSE) {
    ## Look for archived
    action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/out.txt", sep="")
    return(mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE))
  } else {
    return(mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE))
  }
}

