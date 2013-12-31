# Roxygen Comments mantaJob.cancel
#' mantaJob.cancel Sends Manta a cancel message to stop running job
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
mantaJob.cancel <-
function(jobid) {
  if (missing(jobid)) stop("No job identifier provided")
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/cancel", sep="") 
  mantaAttempt(action, method = "POST", returncode = 204,  test = TRUE, json = FALSE)
}
