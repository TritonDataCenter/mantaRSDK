# Roxygen Comments mantaCat
#' Retrieves object from Manta and uses cat() to print contents to the R console.
#'
#' @param mantapath string, required.
#' 
#' @keywords Manta, manta
#'
#' mantaCat is intended for text files, use at your own risk on binary data.
#'
#' @export
mantaCat <-
function(mantapath) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(mantapath)) {
     cat("mantaCat Error - no subdirectory or object name specified\n")
 }

 buffer <- mantaGet(mantapath = mantapath, buffer = TRUE)
 if (buffer[1] == TRUE) cat(rawToChar(buffer))
}
