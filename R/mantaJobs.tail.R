# Roxygen Comments mantaJobs.tail
#' Returns the last run Manta job identifiers, sorted by time.
#'
#' @param n integer. Index into most recently run Jobs list.
#' n = 1 default is last Manta Job run. 2 is second last, and so on.
#'
#' @keywords Manta, manta
#'
#' @export
mantaJobs.tail <-
function(n = 1) {
  jobslist <- mantaLs(mantapath = "~~/jobs", l='names', sortby = 'time')
  if (jobslist[1] == "") {
    stop("No Manta Jobs Found.")
  }
  return( jobslist[(length(jobslist) - n + 1)] )
}
