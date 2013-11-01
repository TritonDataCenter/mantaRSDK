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

 if (missing(mantapath)) {
     cat("mantaRSDK:mantaMkdir Error - no subdirectory specified")
     return(FALSE)
    }

    path_enc <- mantaExpandPath(mantapath)
    if (path_enc == "") {
      # no valid path prefix, assume off working subdir
      path <- paste(mantaGetwd(), mantapath, sep ="/")
      path <- sub("//","/",path) # if user already put in / added by sep above
      path_enc <- mantaExpandPath(path)
    }

    # is it already there?
    if (mantaAttempt(action=path_enc, method="GET", test = TRUE) == TRUE) {
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
