# Roxygen Comments mantaJob.status
#' mantaJob.status returns JSON job status data given Manta job identifier
#'
#' @param jobid character required. Manta job identifier such as
#' "70c30bab-873b-66da-ebc8-ced12bd35ac4"
#'
#' @keywords Manta, manta
#'
#' @export
mantaJob.status <-
function(jobid) {
  if (missing(jobid)) stop("No job identifier provided")
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/status", sep="")
  result <-  mantaAttempt(action, method = "GET", returncode = 204,  json = TRUE)
  ### error handler
  if(result$lines != "") { 
    cat(paste(toJSON(fromJSON(result$lines), pretty=TRUE), "\n", sep=""))
  }
  return(result$lines)
}
