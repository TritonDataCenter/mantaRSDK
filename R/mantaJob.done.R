# Roxygen Comments mantaJob.done
#' Checks or polls status of a Manta job. Returns done or not as logical.
#'
#' @param jobid character optional. Manta job identifier such as
#' \code{"70c30bab-873b-66da-ebc8-ced12bd35ac4"}. Default uses \code{mantaJobs.tail}
#' to fetch the jobid of the last Manta Job run on the service
#'    
#' @param poll logical. Set to \code{TRUE} to poll. Returns \code{FALSE}
#' when poll timeout exceeded and job still running, 
#' \code{TRUE} when job finished.
#'    
#' @param sleep integer. Sleep interval used when polling. Default
#' is 30 seconds.
#' 
#' @param timeout integer. Seconds after which function stops polling.
#' Default is 600 seconds. 
#'    
#' @param silent logical required. Set to \code{TRUE} for non-interactive
#' use of the function.  N.B. Errors are logged to the bunyan
#' buffer. 
#'
#' @return \code{TRUE} when job is no longer running.\cr 
#' \code{FALSE} when job running.\cr 
#' \code{NULL} if job status not found.\cr 
#' N.B. \code{TRUE} return does not imply
#' job success/failure/errors, only running/done state.
#'
#' @family mantaJobs
#'
#' @examples
#' \dontrun{
#' ## Test if last run job is done
#' mantaJob.done()
#' ## Poll a running job till done or timed out.
#' mantaJob.done(poll = TRUE, sleep = 10, timeout = 60)
#' }
#'
#' @keywords Manta, manta
#'
#' @export
mantaJob.done <-
function(jobid, poll = FALSE, sleep = 30, timeout  = 600, silent = FALSE) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  if (missing(jobid)) {
     jobid <- mantaJobs.tail()
  }
  once <- FALSE
  if (poll == TRUE) {
    sleepytime <- 0
    repeat{ 
      done <- mantaJob.done(jobid = jobid, poll = FALSE, silent = silent)
      if (is.null(done)) {  # can't get job status
         return(NULL)
      }
      if (done == TRUE) {  # job is now done
         msg <- paste("\nJob ", jobid, " is Done.\n", sep="")
         bunyanLog.info(msg)
         if (silent == FALSE) { 
           cat(msg)
         }
         return(TRUE)
      }
      if (sleepytime > timeout) { # timed out waiting for job to be done
         msg <- paste("\nmantaJob.done local timeout exceeded. ", jobid, " is Still Running.\n", sep="")
         bunyanLog.info(msg)
         if (silent == FALSE) { 
           cat(msg)
         }
         return(FALSE)
      }
      # here done is FALSE, Job is running
      if (silent == FALSE) { 
          if (once == FALSE) {
            once <- TRUE
            cat("\nPress <esc> key to exit Manta polling mode.\nmantaJob.done(poll=TRUE) to restart.\n\n")
          }
          msg <- paste("Manta Job ", jobid, " Running, R client sleeping for ", sleep, " seconds...\n", sep="")
          cat(msg)
      } 
      sleepytime <- sleepytime + sleep
      Sys.sleep(sleep)
    } # end repeat
  } else {
    statusJSON <- mantaJob.status(jobid, readable = FALSE)
    if (is.null(statusJSON)) {
       msg <- (paste("Job status for ", jobid, " Not Found.\n", sep = ""))
       bunyanLog.error(msg)
       if (silent == FALSE) {
         cat(msg)
       }
      return(NULL)
    }
    if(fromJSON(statusJSON)$state == "done") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

