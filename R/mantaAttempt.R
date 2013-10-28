# TODO: HTTPS HEADER READING/ERROR HANDLING
# Roxygen Comments mantaAttempt
#' REST API Manta Caller with exception handling
#'
#'
#' @param action string, optional. The Manta REST API command 
#' to transmit. If unspecified, uses current Manta Directory
#' and returns JSON listing values for first 256 objects.
#' 
#' @param json logical, optional. Set to FALSE to return R data
#' 
#' @param test logical, optional, Set to TRUE to return logical 
#' pass/fail
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
function(action, json = TRUE, test = FALSE, verbose = FALSE) {

  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  if (missing(action)) {
    manta_do <- manta_globals$manta_cwd
  } else {
    manta_do <- action
  }

  manta_call <- paste(manta_globals$manta_url, manta_do, sep="")

  json_manta_reply <- ""
  json_manta_reply <- getURL(manta_call, 
                             httpheader = mantaGenHeaders(), 
                             verbose = verbose) 

  #### This test and other calls to getURL need to trap 403 and other errors and convert to messages..

  if (test == TRUE) { 
    if (nchar(json_manta_reply) > 0) { 
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  json_lines <- strsplit(json_manta_reply,split="\n") 
  if (json == TRUE) {
    return(json_lines[[1]])
  } else {
    return(lapply(json_lines[[1]],fromJSON))
  }

}
