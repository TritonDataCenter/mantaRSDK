# Roxygen Comments mantaSetwd.reports
#' Sets current Manta working directory to \code{~~/reports}
#'
#' See \code{mantaSetwd}.
#'
#' @keywords Manta, manta
#'
#' @family Directory
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
