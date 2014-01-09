# Roxygen Comments mantaJob.outputs.cat
#' Retrieves list of Manta output objects given Manta job identifier, then
#' retrieves each object from Manta and uses cat() to print contents 
#' to the R console.
#'
#' @param jobid character optional. Manta job identifier such as
#' "70c30bab-873b-66da-ebc8-ced12bd35ac4". Default uses mantaJobs.tail()
#' to fetch the jobid of the last Manta Job run on the service
#'    
#' @keywords Manta, manta
#'
#' @export
mantaJob.outputs.cat <-
function(jobid) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  if (missing(jobid)) {
   jobid <- mantaJobs.tail()
  }
  outputs <- mantaJob.outputs(jobid)
  lapply(outputs,mantaCat)
  cat("\n")
}

