# Roxygen Comments mantaJobs
#' mantaJobs lists ~~/jobs directory
#'
#' @keywords Manta, manta
#'
#' @export
mantaJobs <-
function() {
  return(mantaLs(mantapath = "~~/jobs", l='names', sortby = 'time'))
}
