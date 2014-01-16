# Roxygen Comments mantaRmdir   
#' Removes Manta subdirectory.
#'
#' Removes specified Manta subdirectory. Non-recursive, not vectorized.
#'
#' Removes directory. Specify absolute (e.g. \code{~~/stor/myobject.txt} )
#' or relative path from the current working Manta directory. 
#'
#' @param mantapath character, required. Not vectorized.
#'
#' @param info logical. Set FALSE to suppress console messages.
#'
#' @keywords Manta, manta
#'
#' @return \code{TRUE} or \code{FALSE} depending on success of remove.
#'
#' @family Directory
#'
#' @examples
#' \dontrun{
#' ## Save current working Manta directory
#' mantaGetwd() -> tempdir
#' ## Relative mantapath use:
#' mantaSetwd.stor()
#' mantaMkdir("a_test")
#' mantaLs.l()
#' mantaRmdir("a_test")
#' mantaLs.l()
#' ## Absolute mantapath use:
#' mantaMkdir("~~/public/b_test")
#' mantaLs.l("~~/public")
#' mantaRmdir("~~/public/b_test")
#' mantaLs.l("~~/public")
#' ## Restore current working Manta directory
#' mantaSetwd(tempdir)
#' }
#'
#' @export
mantaRmdir <-
function(mantapath, info = TRUE) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(mantapath)) {
     msg <- "mantaRmdir Error - no Manta object or path to object specified\n"
     bunyanLog.error(msg)
     if (info == TRUE) {
       cat(msg)
     }
     return(FALSE)
 }

 path_enc <- mantaPath(mantapath)
   
 # Don't remove the empty directory you are currently in...
 if (path_enc == mantaExpandPath(mantaGetwd())) {
     msg <- "mantaRmdir Error - Cannot remove directory you are currently in.\n"
     bunyanLog.error(msg)
     if (info == TRUE) {
       cat(msg)
     }
   return(FALSE)
 }

## There are other cases where you are in a child and try to remove a parent,
## but the service logic disallows that. 

 if (path_enc != "") {
   return(mantaAttempt(action=path_enc, method = "DELETE", test = TRUE, returncode = "204"))
 } 
   
 return(FALSE)
}
