# Roxygen Comments mantaGetwd
#' Gets current working directory on Manta.
#' 
#' The current working directory is stored internally in
#' \code{mantaRSDK} on your local system and is not saved.
#' between sessions. It initializes to the root directory
#' of private Manta storage: \code{~~/stor}.
#'
#' @keywords Manta
#'
#' @family Directory
#'
#' @examples
#' \dontrun{
#' ## Manta working directory
#' mantaGetwd() 
#' mantaGetwd() -> tempdir
#' mantaSetwd.public()
#' mantaLs.l()
#' mantaSetwd(tempdir)
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
