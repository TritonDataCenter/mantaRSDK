# Roxygen Comments mantaMkdir
#' Makes a Manta subdirectory
#'
#' @param mantapath string, required.
#'
#' @param p logical, optional. Make all the parent directories too
#'
#' @keywords Manta, manta
#'
#' @export
mantaMkdir <-
function(mantapath, p = FALSE) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(mantapath)) {
     cat("mantaMkdir Error - no subdirectory specified")
     return(FALSE)
 }


 path_enc <- mantaPath(mantapath)

 # is it already there?
 if (mantaAttempt(action=path_enc, method="HEAD", test = TRUE, silent = TRUE) == TRUE) {
      return(TRUE)  
 } 

 if (p == FALSE) {  # no recursion, try it
   headers <- c('content-type' = "application/json; type=directory")
   return(mantaAttempt(action=path_enc, method = "PUT", headers = headers, 
          test = TRUE, returncode = "204"))
 } else { 
   subdirs <- strsplit(path_enc,"/")
   depth <- length(subdirs[[1]]) 
   if (depth < 3) {
      return(TRUE)  # in the 2 root subdirs - stop recursion
   }
   # chop off the last subdir, make the new path
   path <- paste(subdirs[[1]][-length(subdirs[[1]])], collapse="/")
   # Recursively call mantaMkdir with the parent 
   if (mantaMkdir(path, p=p) == TRUE) {  # the parent exists or was just made 
      headers <- c('content-type' = "application/json; type=directory")
      return(mantaAttempt(action=path_enc, method = "PUT", headers = headers, 
             test = TRUE, returncode = "204"))
   }
 }
}
