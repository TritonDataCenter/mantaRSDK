# Roxygen Comments mantaSetwdPublic
#' Sets Manta working directory to ~~/public
#'
#' @keywords Manta, manta
#'
#' @export
mantaSetwdPublic <-
function() {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  manta_cwd <- "~~/public"
  path_enc <- mantaExpandPath(manta_cwd)
  assign("manta_cwd", path_enc, envir=manta_globals)
}
