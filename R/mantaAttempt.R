# Roxygen Comments mantaAttempt
#' REST API Manta Caller with exception handling
#'
#' Given path or object name and current Manta directory
#' returns URL
#'
#' @param action string, optional. The Manta REST API command 
#' to transmit. If unspecified, uses current Manta Directory
#' and returns JSON listing values.
#' 
#' @param json logical, optional. Set to FALSE to return R data
#' 
#' @param verbose logical, optional. Passed to RCurl GetURL, 
#' Set to TRUE to see background REST communication.
#' 
#' @return The Manta reply encoded in JSON or as R data
#'
#' @keywords Manta, manta
#'
#' @export
mantaAttempt <-
function(action, json=TRUE, verbose=FALSE) {

  if (manta_globals$manta_ok == FALSE) {
    mantaInitialize()
  }

  if (missing(action)) {
    manta_do <- manta_globals$manta_cwd
  } else {
    manta_do <- action
  }

  manta_call <- paste(manta_globals$manta_url, manta_do, sep="")

  json_manta_reply <- getURL(manta_call, 
                             httpheader=mantaGenHeaders(), 
                             verbose=verbose)
  json_lines <- strsplit(json_manta_reply,split="\n")
  if (json == TRUE) {
    return(json_lines[[1]])
  } else {
    return(lapply(json_lines[[1]],fromJSON))
  }

}
