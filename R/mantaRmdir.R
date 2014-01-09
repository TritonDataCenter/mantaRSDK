# Roxygen Comments mantaRmdir   
#' Removes Manta subdirectory.
#'
#' Specifiy the full manta path or the just subdirectory name from current
#' working Manta directory.
#' Returns TRUE if object successfully removed 
#'
#' @param mantapath string, required.
#'
#' @keywords Manta, manta
#'
#' @return logical, TRUE if object successfully removed.
#'
#' @export
mantaRmdir <-
function(mantapath) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(mantapath)) {
     cat("mantaRmdir Error - no Manta object or path to object specified")
     return(FALSE)
 }

 path_enc <- mantaPath(mantapath)
   
 # cannot remove an empty directory you are currently in...
 if (path_enc == mantaExpandPath(mantaGetwd())) return(FALSE)

 if (path_enc != "") {
   return(mantaAttempt(action=path_enc, method = "DELETE", test = TRUE, returncode = "204"))
 } 
   
 return(FALSE)
}
