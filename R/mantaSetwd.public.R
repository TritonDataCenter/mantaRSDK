# Roxygen Comments mantaSetwd.public
#' Sets current Manta working directory to \code{~~/public}
#'
#' See \code{mantaSetwd}.
#'
#' @keywords Manta, manta
#'
#' @family Directory
#'
#'
#' @export
mantaSetwd.public <-
function() {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  manta_cwd <- "~~/public"
  path_enc <- mantaExpandPath(manta_cwd)
  assign("manta_cwd", path_enc, envir=manta_globals)
}
