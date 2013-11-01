# Roxygen Comments mantaAttempt
#' REST API Manta Caller with exception handling
#'
#' mantaAttempt uses the current Manta URL
#' and the current working manta directory set with
#' mantaSetwd() and tries to list the directory.
#' If test == TRUE, it returns pass/fail logical
#' which can stat a directory or object. When action is passed
#' a valid path to a Manta object this returns the object
#' JSON metadata.
#'
#' If passed a Manta subdirectory, it returns the directory
#' JSON according to the length limit set with mantaSetLimits()
#'
#' Note getURL verbose = TRUE writes to stderr - invisible 
#' on Windows R. 
#'
#' @param action string, optional. Path to a manta object or
#' directory. When unspecified, uses current Manta Directory
#' and returns JSON listing values for the entire directory.
#' 
#' @param method string, optional. Default is "GET", passed
#' "GET", "POST", "OPTIONS", "PUT", "DELETE" or "HEAD" 
#' from higher level library callers.
#'
#' @param headers, array of named strings, optional. The headers
#' follow the RCurl structure of vector of strings where HTTP 
#' header tags are the names, values as 
#' named strings, no semicolons or delimiters.
#' 
#' @param returncode, string, optional. Set to expected HTTP
#' return code, e.g. 200, 204 - used when test is TRUE
#'
#' @param json logical, optional. Set to FALSE to return R data
#'
#' @param test logical, optional, Set to TRUE to return logical 
#' TRUE/FALSE the request passed or failed. Also
#' tests Manta dns resolution.
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
function(action, method, headers, returncode, json = TRUE, test = FALSE, verbose = FALSE) {

  if (missing(headers)) headers <- NULL
  if (missing(returncode)) returncode <- 0
  
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  # action needs some regexp sanity checking - needs to 
  # have username,  /stor or /public 

  # for now - for Setwd... to be refactored
  if (missing(action)) {
    manta_do <- manta_globals$manta_cwd
  } else {
    manta_do <- action
  } 

  # Note about mapping of HTTP methods to Manta function groups (Node.js names)
  # "GET" mls, mget, mjob (list, get, outputs, errors, failures, inputs)
  # "POST" mjob (create, add, end input, cancel)
  # "PUT" mmkdir, mln, mput
  # "DELETE" mrm, mrmdir
  # "OPTIONS" used for CORS headers
  # "HEAD" possible to use for mls to get directory length

  if (missing(method)) {
      curl_method <- "GET"
  } else {
     curl_method <- method
     if (is.na(charmatch(curl_method,manta_globals$manta_methods)))
      stop("mantaRSDK:mantaAttempt: Error invalid RCURL method. \nPassed [", curl_method, 
           "] , is not in ", manta_globals$manta_methods, "\n" )
  }

  # Use a handle for multiple passes for long directory listings
  curl_handle <- getCurlHandle()

  # not sure - I have to change timeouts depending on methods in use??
  # or more timeout options for getURL
  curl_timeout <- manta_globals$manta_defaults$connect_timeout

  manta_call <- paste(manta_globals$manta_url, manta_do, sep="")

  
  reply <- tryCatch(getURL(manta_call, 
                           curl = curl_handle,
                           httpheader = c(headers, mantaGenHeaders()), 
                           verbose = verbose, 
                           header = TRUE, 
                           customrequest = curl_method,
                           timeout = curl_timeout),
                    COULDNT_RESOLVE_HOST = function(e) {
                           cat("mantaRSDK:mantaAttempt:getURL Cannot Resolve Manta Host at\n ", 
                           manta_globals$manta_url ,"\n")                        
                           },
                    error = function(e) {
                           cat("mantaRSDK:mantaAttempt:getURL HTTP error: ", e$message, "\n")
                          }
                    )

  if (is.null(reply)) {    # there was no reply from the server 
    if (test == TRUE) {
      return( FALSE )
    } else {  # empty string
      return("")
    }
  }


  # Something was returned from server
  split_reply <- strsplit(reply, split = "\r\n\r\n")
  header <- split_reply[[1]][1]
  body <- split_reply[[1]][-1] # in R this removes the first element in the array
  header_lines <- strsplit(header, split= "\r\n")
  no_body <- FALSE
  if (!length(body)==0) {
   body_lines <- strsplit(body[[1]], split = "\n")
  } else {
   body_lines <- c("")
   no_body <- TRUE
  }
  returned_code = ""
  returned_string <- header_lines[[1]][ charmatch("HTTP", header_lines[[1]]) ] 
  returned_code <- strsplit(returned_string, split=" ")[[1]][2]


  # for HTTP calls like DELETE that return no data.
  if ((returncode != 0) && (test == TRUE)) {
    if (as.integer(returned_code) == returncode) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # Server Error Responses
  if (as.integer(returned_code) >= 400) {
    if (isValidJSON(body_lines[[1]], asText = TRUE)) {
      values <- fromJSON(body_lines[[1]])

      # this checks the error strings to see if it is on the list...
      if (sum(charmatch(values, manta_globals$manta_error_classes, nomatch = 0)) > 0) {
        cat("mantaRSDK:mantaAttempt Manta Service Error: ")
      } else {
        cat("mantaRSDK:mantaAttempt Unknown Error Class: ")
      }

      # It was valid JSON, so show it as the return error message
      cat(values)
      cat("\n") 
    } else {  
      # not valid JSON returned, just return the error code...
      cat(paste("mantaRSDK:mantaAttempt Unrecognized - Server Error Code: ", returned_string, "\n", sep=" "))         
    } 
   
    if (test == TRUE) { 
      return(FALSE) 
    } else {
      return("")
    }
  }
    
  # OK, so no HTTP, resolve, server errors reported, some response received, 
  # Sanity check to see that response is JSON 
  if (no_body == FALSE) {
    if (isValidJSON(body_lines[[1]], asText = TRUE) == FALSE) {
      cat("mantaRSDK:mantaAttempt Error - Server Response is not parseable JSON using RJSONIO: \n")
      cat(body_lines[[1]][1],"\n")
      cat("\n")
      if (test == TRUE) { 
        return(FALSE)
      } else {
        # We grind to a halt without valid JSON response, some fix required
        stop("Unable to process response.\n")
      }
    }
  }
  
  # So we made it through, for testing purposes
  if (test == TRUE) { 
   return(TRUE) 
  }

    
  # For Manta Subdirectory listings - get the number of entries
  result_set_count <- 0
  result_set_size <- header_lines[[1]][ charmatch("Result-Set-Size",header_lines[[1]]) ]
  if (nchar(result_set_size) != 0) {
    result_set_count <- as.integer(strsplit(result_set_size, split=" ")[[1]][2])
  }

    # use >= 0 here? 
    #  if ((result_set_count > 0) && (result_set_count <= 256)) {
    #  # nothing for the moment
    #  } else {
    #    cat("More than 256 entries, gather up to some limit\n")
    #  }

  if ((json == TRUE) || (no_body ==TRUE)) {
    return(body_lines[[1]])
  } else {
    return(lapply(body_lines[[1]],fromJSON))
  }

}
