# Roxygen Comments mantaGetwd
#' Gets Manta working directory.
#'
#' @keywords Manta, manta
#'
#' @family Directory
#'
#' @examples
#' \dontrun{
#' ## Manta working directory
#' mantaGetwd()
#' }
#'
#' @export
mantaGetwd <-
function() {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  cwd <- manta_globals$manta_cwd
  return(curlUnescape(cwd))
}
