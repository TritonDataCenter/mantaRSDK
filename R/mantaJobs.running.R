# Roxygen Comments mantaJobs.running
#' mantaJobs lists ~~/jobs directory
#'
#' @keywords Manta, manta
#'
#' @export
mantaJobs.running <-
function() {
  return(mantaLs(mantapath = "~~/jobs?state=running", l='names', sortby = 'time'))
}
