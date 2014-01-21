# Roxygen Comments mantaJob.inputs
#' Returns list of input Manta objects given Manta job identifier.
#'
#' @inheritParams mantaJob.outputs
#'
#' @keywords Manta
#'
#' @family mantaJobs
#'
#' @examples
#' \dontrun{
#' ## See the list of of the last run Manta job inputs:
#' mantaJob.inputs()
#' }
#'
#' @export
mantaJob.inputs <-
function(jobid) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  if (missing(jobid)) {
    jobid <- mantaJobs.tail()
  }
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

