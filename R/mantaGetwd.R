# Roxygen Comments mantaGetwd
#' Gets Manta working directory, removes space encoding
#'
#' @keywords Manta, manta
#'
#' @export
mantaGetwd <-
function() {
  cwd <- manta_globals$manta_cwd
  return (cwd)
}
