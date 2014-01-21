# Roxygen Comments mantaJobs
#' Lists all Manta job identifiers, sorted by time.
#'
#' Clean out your Manta job directory regularly to avoid
#' paying storage costs and having the archive grow to large
#' numbers of files.
#'
#' @keywords Manta
#'
#' @family mantaJobs
#'
#' @examples
#' \dontrun{
#' ## List all the Manta jobs you have run so far:
#' mantaJobs()
#' }
#'
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
