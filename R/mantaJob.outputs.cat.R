# Roxygen Comments mantaJob.outputs.cat
#' Prints contents of all job output objects to the R console.
#' 
#' Avoid using this on binary output data.
#'
#' @inheritParams mantaJob.outputs
#'
#' @keywords Manta
#'
#' @family mantaJobs
#'
#' @examples
#' \dontrun{
#' ## Print all the output files to the console.
#' mantaJob.outputs.cat()
#' }
#'
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

