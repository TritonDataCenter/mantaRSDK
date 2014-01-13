# Roxygen Comments mantaMkdir
#' Makes a Manta subdirectory, optionally with parent directories.
#'
#' @param mantapath string, required.
#'
#' @param p logical, optional. Make all the parent directories too
#'
#' @param info logical. Set FALSE to suppress console messages. 
#'
#' @keywords Manta, manta
#'
#' @export
mantaMkdir <-
function(mantapath, p = FALSE, info = TRUE) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(mantapath)) {
     if (info == TRUE)   cat("mantaMkdir Error - no subdirectory specified")
     return(FALSE)
 }



 # Already there?
 if (mantaExists(mantapath, d = TRUE)) {
      msg <- paste("mantaMkdir: Directory already exists: ", mantapath, "\n", sep = "")
      bunyanLog.info(msg)
      if (info == TRUE) cat(msg)
      return(TRUE)
  }


 path_enc <- mantaPath(mantapath)

 silent <- !info

 if (p == FALSE) {  # no recursion, try it
   headers <- c('content-type' = "application/json; type=directory")
   return(mantaAttempt(action=path_enc, method = "PUT", headers = headers, 
          test = TRUE, returncode = "204"))
 } else { 
   # Fixed double escaping...
   subdirs <- strsplit(curlUnescape(path_enc),"/")
   depth <- length(subdirs[[1]]) 
   if (depth < 3) {
      return(TRUE)  # in the 2 root subdirs - stop recursion
   }
   # chop off the last subdir, make the new path
   path <- paste(subdirs[[1]][-length(subdirs[[1]])], collapse="/")
   # Recursively call mantaMkdir with the parent 
   if (mantaMkdir(path, p=p, info = info) == TRUE) {  # the parent exists or was just made 
      headers <- c('content-type' = "application/json; type=directory")
      return(mantaAttempt(action=path_enc, method = "PUT", headers = headers, 
             test = TRUE, silent = silent,  returncode = "204"))
   }
 }
}
