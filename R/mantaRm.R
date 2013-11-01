# Roxygen Comments mantaRm
#' Removes Manta object specified by full manta path or from current
#' working manta directory
#' Returns TURE if object successfully removed
#'
#' @param mantapath string, required.
#'
#' @keywords Manta, manta
#'
#' @export
mantaRm <-
function(mantapath) {
 if (missing(mantapath)) {
     cat("mantaRSDK:mantaRm Error - no Manta object or path to object specified")
     return(FALSE)
    }

    path_enc <- mantaExpandPath(mantapath)
    if (path_enc == "") { 
      # no valid path prefix, see if this is an object assuming the cwd prefix
      path <- paste(mantaGetwd(), mantapath, sep ="/")
      path <- sub("//","/",path) # if user already put in / added by sep above
      path_enc <- mantaExpandPath(path)
    }
   
    if (path_enc != "") {
      return(mantaAttempt(action=path_enc, method="DELETE", test = TRUE, returncode="204"))
    } 
   
    return(FALSE)
}
