# Roxygen Comments mantaJobs.running
#' Lists identifiers of any running Manta jobs.
#'
#' @keywords Manta, manta
#'
#' @export
mantaJobs.running <-
function() {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }  
  return(mantaLs(mantapath = "~~/jobs?state=running", l='names', sortby = 'time'))
}
