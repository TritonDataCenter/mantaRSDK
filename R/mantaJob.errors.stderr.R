# Roxygen Comments mantaJob.errors.stderr
#' Retrieves JSON errors given Manta job identifier, then retrieves each stderr message 
#' archived on Manta (if any) and uses mantaCat() to print contents of stderr 
#' to the console.
#'
#' @param jobid character optional. Manta job identifier such as
#' "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default uses mantaJobs.tail()
#' to fetch the jobid of the last Manta Job run on the service
#'    
#' @keywords Manta, manta
#'
#' @export
mantaJob.errors.stderr <-
function(jobid) {
  if (missing(jobid)) {
   jobid <- mantaJobs.tail()
  }
  jsonerrs <- mantaJob.errors(jobid, readable = FALSE)
  errs <- unlist(lapply(jsonerrs, fromJSON))
  stderrs <- as.character(errs[names(errs) == "stderr"])
  lapply(stderrs,mantaCat)
  cat("\n")
}

