# Roxygen Comments mantaRmdir   
#' Removes Manta subdirectory specified by full manta path or from current
#' working manta directory
#' Returns TURE if object successfully removed 
#'
#' @param mantapath string, required.
#'
#' @keywords Manta, manta
#'
#' @export
mantaRmdir <-
function(mantapath) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(mantapath)) {
     cat("mantaRSDK:mantaRmdir Error - no Manta object or path to object specified")
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
