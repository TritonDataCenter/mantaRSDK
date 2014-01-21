# Roxygen Comments mantaMkdir
#' Makes a Manta subdirectory, optionally with parent directories.
#'
#' Makes subdirectory on Manta. Specify absolute (e.g. \code{~~/stor} )
#' or relative path from the current Manta directory. Supports 
#' creation of parent directories with \code{p = TRUE}. New
#' directories can be created in \code{~~/stor} your private space or
#' \code{~~/public} a publically accessible HTTPS directory.
#'
#' @param mantapath character, required. Path or name of new 
#' subdirectory to create. Not vectorized. 
#'
#' @param p logical, optional. Make all the parent directories too.
#'
#' @param info logical. Set FALSE to suppress console messages. 
#'
#' @keywords Manta
#'
#' @return \code{TRUE} or \code{FALSE} depending on success of upload.
#'
#' @family Directory
#'
#' @seealso \code{\link{mantaRmdir}}
#'
#' @examples
#' \dontrun{
#' ## Make absolute path subdirectory
#' mantaMkdir("~~/stor/testdirectory")
#' mantaLs.l("~~/stor")
#' mantaRmdir("~~/stor/testdirectory")
#'
#' ## Make with parent directories, 
#' mantaGetwd() -> tempdir
#' mantaMkdir("~~/stor/a_test/b_test/c_test", p = TRUE) 
#' mantaSetwd("~~/stor/a_test/b_test/c_test")
#' mantaMkdir("d_test")   # Relative path
#' data <- runif(100)
#' mantaDump("data")
#' mantaSetwd("..")
#' mantaDump("data")
#' mantaSetwd("..")
#' mantaDump("data")
#' mantaLs.l()
#' mantaFind()

#' ## Recursive Rm Subdirectory Contents
#' mantaSetwd.stor()
#' mantaRm("~~/stor/a_test", r = TRUE)
#' mantaSetwd("~~/stor/a_test")
#' mantaLs.l()
#' mantaFind()
#' mantaSetwd.stor()
#' mantaRmdir("~~/stor/a_test")
#' mantaLs.l("~~/stor")
#' mantaSetwd(tempdir)
#' }
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
