# Roxygen Comments mantaJobs.running
#' Lists running Manta job identifiers
#'
#' @keywords Manta, manta
#'
#' @export
mantaJobs.running <-
function() {
  return(mantaLs(mantapath = "~~/jobs?state=running", l='names', sortby = 'time'))
}
