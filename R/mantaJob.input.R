# Roxygen Comments mantaJob.input
#' Returns list of input objects given Manta job identifier
#'
#' @param jobid character required. Manta job identifier such as
#' "70c30bab-873b-66da-ebc8-ced12bd35ac4"  or use mantaJob.last()
#' to fetch the jobid of the last manta Job run on the service
#' e.g. mantaJob.errors(mantaJob.last())
#'    

#'
#' @keywords Manta, manta
#'
#' @export
mantaJob.input <-
function(jobid) {
  if (missing(jobid)) stop("No job identifier provided")
  ## Look for live/err
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/in", sep="") 
  result <-  mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE, silent = TRUE, test = TRUE)
  if (result == FALSE) {
    ## Look for archived
    action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/in.txt", sep="")
    return(mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE))
  } else {
    return(mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE))
  }
}

