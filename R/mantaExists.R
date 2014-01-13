# Roxygen Comments mantaExists
#' Tests to see if a Manta object or subdirectory exists.
#'
#' @param mantapath character, required.
#'
#' @param d logical. Set TRUE to confirm that entity specified in mantapath 
#' exists and is a directory.
#'
#' @return logical.
#'
#' @keywords Manta, manta
#'
#' @export
mantaExists <-
function(mantapath, d = FALSE) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(mantapath)) {
     cat("mantaExists Error - no subdirectory or object name specified")
     return(FALSE)
 }

 path_enc <- mantaPath(mantapath)
 retval <- mantaAttempt(action=path_enc, method="HEAD", test = TRUE, silent = TRUE)

 if (d == TRUE) {
   if (retval == TRUE) {
     metadata <- mantaGet(mantapath, metadata = TRUE, info = FALSE)
     if (length(metadata) == 2) {
       if (grepl("type=directory", paste(metadata$metadata, collapse = "\n")) != TRUE) {
        retval <- FALSE
       }
     }
   }
 }

 return(retval)
}
