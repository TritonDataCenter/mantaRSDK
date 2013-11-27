# Roxygen Comments mantaSetwd.reports
#' Sets Manta working directory to ~~/reports
#'
#' @keywords Manta, manta
#'
#' @export
mantaSetwd.reports <-
function() {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  manta_cwd <- "~~/reports"
  path_enc <- mantaExpandPath(manta_cwd)
  assign("manta_cwd", path_enc, envir=manta_globals)
}
