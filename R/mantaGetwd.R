# Roxygen Comments mantaGetwd
#' Gets Manta working directory, removes space encoding
#'
#' @keywords Manta, manta
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
