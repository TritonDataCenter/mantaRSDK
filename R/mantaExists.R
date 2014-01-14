# Roxygen Comments mantaExists
#' Tests to see if a Manta object or subdirectory exists. 
#'
#' Like a Unix \code{stat} command. Does not download object contents. 
#'
#' @param mantapath character, required. A full path specifying a Manta object
#' or directory, or the name of an object/subdir in the current Manta working
#' directory.
#'
#' @param d logical. Set \code{TRUE} to confirm that entity specified in mantapath 
#' exists and is a directory.
#'
#' @return logical.
#'
#' @keywords Manta, manta
#'
#' @family mantaLs
#'
#' @examples
#' \dontrun{
#' data <- runif(100)
#' mantaDump("data")
#' mantaExists("dumpdata.R")
#' mantaRm("dumpdata.R")
#' mantaExists("dumpdata.R")
#' mantaMkdir("testsubidrectory")
#' mantaExists("testsubdirectory", d = TRUE)
#' mantaRmdir("testsubdirectory")
#' mantaExists("testsubdirectory", d = TRUE)
#' files <- c("file1", "file2", "file3")
#' sapply(files, mantaExists)
#' }
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
