# Roxygen Comments mantaJob.failures
#' mantaJob.failures returns list of failures given Manta job identifier
#'
#' @param jobid character required. Manta job identifier such as
#' "70c30bab-873b-66da-ebc8-ced12bd35ac4" 
#'
#' @keywords Manta, manta
#'
#' @export
mantaJob.failures <-
function(jobid) {
  if (missing(jobid)) stop("No job identifier provided")
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/fail", sep="")
  result <-  mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE)
  if (is.list(result)) return(result)
  return(list(count = length(result), lines = result))

}

