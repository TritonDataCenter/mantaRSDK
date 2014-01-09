# Roxygen Comments mantaExists
#' Tests to see if a Manta object or subdirectory exists.
#'
#' @param mantapath character, required.
#'
#' @return logical.
#'
#' @keywords Manta, manta
#'
#' @export
mantaExists <-
function(mantapath) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(mantapath)) {
     cat("mantaExists Error - no subdirectory or object name specified")
     return(FALSE)
 }
 path_enc <- mantaPath(mantapath)
 return(mantaAttempt(action=path_enc, method="HEAD", test = TRUE, silent = TRUE))
}
