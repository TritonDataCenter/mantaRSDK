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
 if (missing(mantapath)) {
     cat("mantaRSDK:mantaRmdir Error - no Manta object or path to object specified")
     return(FALSE)
    }

    path_enc <- mantaExpandPath(mantapath)
    if (path_enc == "") {            
      # no valid path prefix, see if this is a subdir assuming the cwd prefix
      path <- paste(mantaGetwd(), mantapath, sep ="/")
      path <- sub("//","/",path) # if user already put in / added by sep above
      path_enc <- mantaExpandPath(path)
    }
   
    # cannot remove an empty directory you are currently in...
    if (path_enc == mantaExpandPath(mantaGetwd())) return(FALSE)

    if (path_enc != "") {
      return(mantaAttempt(action=path_enc, method = "DELETE", test = TRUE, returncode = "204"))
    } 
   
    return(FALSE)
}
