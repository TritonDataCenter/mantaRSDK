# Roxygen Comments mantaJob.cancel
#' Sends Manta a cancel message to stop running job.
#'
#' @param jobid character optional. Manta job identifier such as
#' \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}. Default uses \code{mantaJobs.tail}
#' to fetch the jobid of the last Manta Job run on the service.
#'
#' @keywords Manta, manta
#'
#'
#' @family mantaJobs
#'
#' @examples
#' \dontrun{
#' # Send cancel signal for last run Manta job.
#' mantaJob.cancel()
#' }

#'
#' @export
mantaJob.cancel <-
function(jobid) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  if (missing(jobid)) {
    jobid <- mantaJobs.tail()
  }
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/cancel", sep="") 
  mantaAttempt(action, method = "POST", returncode = 204,  test = TRUE, json = FALSE)
}
