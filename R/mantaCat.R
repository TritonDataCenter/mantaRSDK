# Roxygen Comments mantaCat
#' Retrieves object from Manta and uses \code{cat} to print contents to the R console.
#' \code{mantaCat} is intended for text files, use at your own risk on binary data.
#'
#' @param mantapath vector of character, required. 
#' 
#' @param sep character, required, separator.
#'
#' @keywords Manta manta
#'
#' @family mantaGet
#'
#' @seealso \code{\link{mantaJob.outputs.cat}}, \code{\link{mantaJob.errors.stderr}}
#'
#' @examples
#' \dontrun{
#' data <- runif(100) 
#' mantaDump("data")
#' mantaCat("dumpdata.R")
#' mantaRm("dumpdata.R")
#' }

#' @export
mantaCat <-
function(mantapath, sep ="\n") {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(mantapath)) {
     cat("mantaCat Error - no subdirectory or object name specified\n")
 }

 if (length(mantapath) > 1){
   lapply(mantapath, mantaCat) 
 } else {
   buffer <- mantaGet(mantapath = mantapath, buffer = TRUE, info = FALSE)
   if (buffer[1] == TRUE) cat(rawToChar(buffer))
 }
 cat(sep)
}
