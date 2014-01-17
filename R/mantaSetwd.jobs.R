# Roxygen Comments mantaSetwd.jobs
#' Sets Manta working directory to \code{~~/jobs}
#'
#' @keywords Manta, manta
#'
#' @export
mantaSetwd.jobs <-
function() {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  manta_cwd <- "~~/jobs"
  path_enc <- mantaExpandPath(manta_cwd)
  assign("manta_cwd", path_enc, envir=manta_globals)
}
