# Roxygen Comments mantaJob.errors.stderr
#' Retrieves stderr messages given  Manta job identifier.
#'
#' When you run a Manta job, any errors that the UNIX command attempted
#' that are written to stderr are archived. This function retrieves
#' the JSON errors messages and then all stderr message archives for 
#' each process, then uses mantaCat() to print contents of stderr 
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
  if (length(stderrs) > 0 ) mantaCat(stderrs, sep = "\n")

}

