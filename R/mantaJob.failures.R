# Roxygen Comments mantaJob.failures
#' Returns list of failures given Manta job identifier.
#'
#' @inheritParams mantaJob.outputs
#'
#' @keywords Manta, manta
#'
#' @family mantaJobs
#'
#' @examples
#' \dontrun{
#' ## Check if the last run Manta job had failures:
#' mantaJob.failures()
#' }
#'
#' @export
mantaJob.failures <-
function(jobid) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  if (missing(jobid)) {
    jobid <- mantaJobs.tail()
  }
  ## Look for live/err
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/fail", sep="")
  result <-  mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE, silent = TRUE, test = TRUE)
  if (result == FALSE) {
    ## Look for archived
    action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/fail.txt", sep="")
    return(mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE))
  } else {
    return(mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE))
  }
}



