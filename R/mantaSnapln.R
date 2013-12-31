# Roxygen Comments mantaSnapln
#' Makes Snaplinks - combination snapshots and symbolic links. 
#'
#' If the original object is overwritten/deleted, the SnapLink
#' still contains the object contents at time of creation.
#'
#' @param from string, required. Object in current subdirectory
#' or full Manta path to stored object.
#'
#' @param to string, required. Snaplink name in current subdirectory
#' or full Manta path to the new SnapLink..
#'
#' @keywords Manta, manta
#'
#' @export
mantaSnapln <-
function(from, to) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(from) || missing(to)) {
   cat("mantaSnapln Error - Missing argument - from or to")
   return(FALSE)
 }

 from_path_enc <- mantaPath(from)

 to_path_enc <- mantaPath(to)

 if ((from_path_enc != "") && (to_path_enc != "")) {
   headers <- c('content-type' = "application/json; type=link",
                'Location' = from_path_enc)
   return(mantaAttempt(action=to_path_enc, method = "PUT", headers = headers, test = TRUE, returncode = "204"))
 }

 return(FALSE)
}

