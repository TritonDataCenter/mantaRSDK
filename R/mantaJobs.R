# Roxygen Comments mantaJobs
#' Lists all Manta job identifiers, sorted by time.
#'
#' @keywords Manta, manta
#'
#' @export
mantaJobs <-
function() {
  return(mantaLs(mantapath = "~~/jobs", l='names', sortby = 'time'))
}
