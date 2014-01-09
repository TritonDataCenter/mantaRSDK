# Roxygen Comments mantaJobs
#' Lists all Manta job identifiers, sorted by time.
#'
#' @keywords Manta, manta
#'
#' @export
mantaJobs <-
function() {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }  
  return(mantaLs(mantapath = "~~/jobs", l='names', sortby = 'time'))
}
