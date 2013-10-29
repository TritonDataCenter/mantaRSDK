# TODO: HTTPS HEADER READING/ERROR HANDLING
# Roxygen Comments mantaAttempt
#' REST API Manta Caller with exception handling
#'
#' Note getURL verbose = TRUE writes to stderr - invisible 
#' on Windows R.
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

  reply <- ""
  reply <- getURL(manta_call, 
                             httpheader = mantaGenHeaders(), 
                             verbose = verbose, 
                             header = TRUE) 

  if (length(reply) == 0)   stop("mantaAttempt:getURL - no reply\n")

  split_reply <- strsplit(reply, split = "\r\n\r\n")
  header <- split_reply[[1]][1]
  body <- split_reply[[1]][-1] # in R this removes the first element in the array
  header_lines <- strsplit(header, split= "\r\n")
  body_lines <- strsplit(body[[1]], split = "\n")


  return_code = ""
  return_string <- header_lines[[1]][ charmatch("HTTP", header_lines[[1]]) ]
  return_code <- strsplit(return_string, split=" ")[[1]][2]
  if (as.integer(return_code) >= 400) {
    cat(paste("mantaRSDK:mantaAttempt:Manta Server Error Code: ", return_string, "\n", sep=" "))
    if (test == TRUE) { return(FALSE) }
  }

  result_set_count <- 0
  result_set_size <- header_lines[[1]][ charmatch("Result-Set-Size",header_lines[[1]]) ]
  result_set_count <- as.integer(strsplit(result_set_size, split=" ")[[1]][2])

  if (test == TRUE) { 
      return(TRUE)
  }

  if ((result_set_count > 0) && (result_set_count <= 256)) {
    if (json == TRUE) {
      return(body_lines[[1]])
    } else {
      return(lapply(body_lines[[1]],fromJSON))
    }
  } else {
    cat("More than 256 entries, gather up to some limit\n")
  }

}
