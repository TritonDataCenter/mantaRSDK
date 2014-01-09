# Roxygen Comments mantaJobs.tail
#' Returns identifier of last run Manta job identifier, or from offset n up from end of list.
#'
#' @param n integer. Index into most recently run Jobs list.
#' n = 1 default is last Manta Job run. 2 is second last, and so on.
#'
#' @keywords Manta, manta
#'
#' @export
mantaJobs.tail <-
function(n = 1) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  jobslist <- mantaLs(mantapath = "~~/jobs", l='names', sortby = 'time')
  if (jobslist[1] == "") {
    stop("No Manta Jobs Found.")
  }
  return( jobslist[(length(jobslist) - n + 1)] )
}
