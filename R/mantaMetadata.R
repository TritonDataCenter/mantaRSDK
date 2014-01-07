# Roxygen Comments mantaMetadata
#' Returns the HTTP HEAD metadata for a Manta object or subdirectory
#'
#' @param mantapath string, required.
#' 
#' @return character array of HTTP HEAD contents.
#'
#' @keywords Manta, manta
#'
#' @export
mantaMetadata <-
function(mantapath) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(mantapath)) {
     cat("mantaMetadata Error - no subdirectory or object name specified")
     return(FALSE)
 }
 path_enc <- mantaPath(mantapath)
 return(mantaAttempt(action=path_enc, method="HEAD"))
}
