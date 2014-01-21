# Roxygen Comments mantaJob.outputs
#' Returns list of output Manta objects given Manta job identifier.
#'
#' Outputs have hashed file names as saved by the Manta service.
#'
#' @param jobid character optional. Manta job identifier such as
#' \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}. Default uses \code{mantaJobs.tail}
#' to fetch the jobid of the last Manta Job run on the service.
#'    
#' @param silent logical required. Set to \code{TRUE} for non-interactive
#' use of the function to suppress \code{stop} on Manta Service error messages,
#' and return an empty data set. N.B. Errors are logged and in the bunyan
#' buffer. 
#'
#' @family mantaJobs
#'
#' @examples
#' \dontrun{
#' ## List the outputs of the last job run on Manta:
#' mantaJob.outputs()
#' ## Download the output files of the last job run on Manta, to current R working
#' ## directory, keeping hashed filenames.
#' mantaGet(mantaJob.outputs()) 
#' }
#'
#' @keywords Manta
#'
#' @export
mantaJob.outputs <-
function(jobid, silent = FALSE) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  if (missing(jobid)) {
   jobid <- mantaJobs.tail()
  }
  ## Look for live/err
  action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/live/out", sep="")
  result <-  mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE, silent = TRUE, test = TRUE)
  if (result == FALSE) {
    ## Look for archived
    action <- paste("/",manta_globals$manta_user,"/jobs/",jobid,"/out.txt", sep="")
    return(mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE))
  } else {
    return(mantaAttempt(action, method = "GET", returncode = 200,  json = FALSE))
  }
}

