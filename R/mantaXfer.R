#TODO handle multiple GETs to same filename by appending (n) to filename...

# Roxygen Comments mantaXfer
#' raw REST API Manta Caller for mantaPut mantaGet and related data transfer routines.
#'
#' Note getURL verbose = TRUE writes to stderr - invisible 
#' on Windows R. 
#'
#' @param action string, optional. Path to a manta object.
#' 
#' @param method string, required. "GET", or "PUT" or "HEAD"
#'
#' @param filename optional. Path to local file for PUT or GET
#'
#' @param buffer optional. Raw buffer to put. 
#'
#' @param returnmetadata logical required. For GET function returns metadata.
#'
#' @param returnbuffer logical required. For GET function returns buffer.
#'
#' @param md5 logical optional. Test md5 hash of data before/after transfer
#'
#' @param headers, array of named strings, optional. The headers
#' follow the RCurl structure of vector of strings where HTTP 
#' header tags are the names, values as 
#' named strings, no semicolons or delimiters.
#' 
#' @param verbose logical, optional. Passed to RCurl GetURL, 
#' Set to TRUE to see background REST communication on stderr
#' which is invisible on Windows
#' 
#' @return TRUE or FALSE depending on success of PUT transfer
#' on GET buffer=TRUE it returns the downloaded buffer
#'
#' @keywords Manta, manta
#'
#' @export
mantaXfer <-
function(action, method, filename, buffer, returnmetadata = FALSE, returnbuffer = FALSE, 
         md5 = FALSE, headers, verbose = FALSE) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }


  if (missing(headers)) headers <- NULL
  if (missing(action)) stop("mantaXfer: No Manta object specified")
  if (missing(filename) && missing(buffer)) {
   stop("mantaXfer: Missing local file information")
  }
  
  if (missing(method)) {
      stop("mantaXfer - method argument not specified")
  } else {
     curl_method <- method
     if (is.na(charmatch(curl_method, c("PUT", "GET", "HEAD")))) {
       msg <- paste("mantaXfer: Error invalid RCURL method. \nPassed [", curl_method, 
           "] , is not PUT or GET or HEAD\n", sep="" )
       bunyanLog.error(msg)
       stop(msg)
     }
  }
  if (curl_method == "HEAD") {
   return(mantaAttempt(action, method = curl_method)) 
  }

  manta_call <- paste(manta_globals$manta_url, action, sep="")

  if (curl_method == "GET")  {
    returncode <- 200
    if (!missing(filename)) {
      if (file.exists(filename) == TRUE) {
        msg <- paste("mantaXfer - File to GET already exists:", filename, "\n", sep="")
        bunyanLog.info(msg = msg)
#TODO                                            ### paste (N) to end of filename...??
      } 
    } else {
      filename <- tempfile()
    }
    curl_handle <- getCurlHandle()
    httpheader <- mantaGenHeaders() 
    req <- list(url = manta_call, method = curl_method, headers = httpheader)
    bunyanLog.debug(msg ="getURL", req = req, version = manta_globals$RSDK_version) 
    buf <- binaryBuffer()
    reply <- tryCatch(getURL(url = manta_call, 
                           curl = curl_handle,
                           httpheader = httpheader,
                           verbose = verbose, 
                           header = TRUE,
                           customrequest = curl_method,
                           write = getNativeSymbolInfo("R_curl_write_binary_data")$address,
                           file = buf@ref,
                           .encoding = 'UTF-8'),
                    COULDNT_RESOLVE_HOST = function(e) {
                           msg <- paste("mantaXfer GET Cannot Resolve Manta Host at\n ", 
                           manta_globals$manta_url ,"\n",sep="")
                           bunyanLog.error(msg = msg, version = manta_globals$RSDK_version) 
                           stop(msg)                        
                           },
                    error = function(e) {
                           msg <- paste("mantaXfer GET HTTP or RCURL error: ", e$message, "\n", sep="")
                           bunyanLog.error(msg = msg, version = manta_globals$RSDK_version)
                           stop(msg) 
                          }
                    )
    b <- as(buf,"raw")

## To get the header and binary data in one pass, we need to chop the header off manually
## Need to find the \r\n\r\n pattern within the first 8K bytes of buffer.
    head_break <- c("0d", "0a", "0d", "0a")
    m = length(head_break)
    n = length(b)
    if (n > 8192) n <- 8192   
    # fast matching subvector courtesy of discussion here: 
    # http://r.789695.n4.nabble.com/matching-a-sequence-in-a-vector-td4389523.html
    candidate <- seq.int(length.out = n - m + 1)
    for (i in seq.int(length.out = m)) {
      candidate <- candidate[head_break[i] == b[candidate + i - 1 ]]
    }
    split_index <- candidate[1] - 1
    header_all <- rawToChar(b[1:split_index])  # this extracts the header as text
## log some error if this is garbage
    header_end <- candidate[1] + 3
    b <- b[-(1:header_end)]  # this removes the header bytes from the buffer
    header_lines <- strsplit(header_all, split= "\r\n")
    returned_string <- header_lines[[1]][ charmatch("HTTP", header_lines[[1]]) ]
  } else {  
    # it is a PUT
    returncode <- 204
    filetemp <- ""
    if (missing(filename)) filename = ""
    if (!missing(buffer)) { # we have a buffer      
        # RCURL stock does not read from a buffer 
        # must write to tempfile
        # these don't work: 
        #                 f <-  file(buffer, "rb", raw = TRUE)
        #                  con <- rawConnection(buffer, open = "rb")
        filetemp <- tempfile()
        f <- file(filetemp, "wb")
        fsize <- length(buffer)
        writeBin(object =  buffer, con = f)
        flush(f)
        close(f)
        f <- CFILE(filetemp, "rb")
    }
    if (filename != "") { # we have a file to read supplied
       if (file.exists(filename) != TRUE) {
          msg <- paste("mantaXfer - File to PUT not found at:", filename, "\n", sep="")
          bunyanLog.error(msg = msg)
          stop(msg)
        }      
       f <- CFILE(filename,"rb")
       fsize <- file.info(filename)[1, "size"]
    } else { # use the tempfile
      filename <- filetemp
    }
    if (md5 == TRUE) {
       openssl_cmd <- "openssl"
       digest_args <- paste("dgst -md5 -binary",
                           "-out temp_digest.bin",
                           sep=" ")
       encrypt_args <- "enc -base64 -in temp_digest.bin"
       system2(openssl_cmd, args=digest_args, stdin=filename, stdout = FALSE)
       md5hash <- paste(system2(openssl_cmd, args=encrypt_args, stdout = TRUE), collapse = '')
       headers <- c(headers, 'content-md5' = md5hash)
    }
    curl_handle <- getCurlHandle()
    httpheader <- c(headers, mantaGenHeaders())
    req <- list(url = manta_call, method = curl_method, headers = httpheader)
    bunyanLog.debug(msg ="getURL", req = req, version = manta_globals$RSDK_version) 
    reply <- tryCatch(getURL(manta_call, 
                           curl = curl_handle,
                           httpheader = httpheader,
                           verbose = verbose, 
                           header = TRUE,
                           upload = TRUE,
                           customrequest = curl_method,
                           readdata = f@ref,
                           infilesize = fsize, 
                           .encoding = 'UTF-8'),
                    COULDNT_RESOLVE_HOST = function(e) {
                           msg <- paste("mantaXfer PUT Cannot Resolve Manta Host at\n ", 
                           manta_globals$manta_url ,"\n",sep="")
                           bunyanLog.error(msg = msg, version = manta_globals$RSDK_version)
                           stop(msg)
                           },
                    error = function(e) {
                           msg <- paste("mantaXfer PUT HTTP or RCURL error: ", e$message, "\n", sep="")
                           bunyanLog.error(msg = msg, version = manta_globals$RSDK_version) 
                           stop(msg)
                          }
                    )
  
    split_reply <- strsplit(reply, split = "\r\n\r\n")
    header <- split_reply[[1]][1]
    body <- split_reply[[1]][-1] # in R this removes the first element in the array
    header_lines <- strsplit(header, split= "\r\n")
    if (!length(body)==0) {
      body_lines <- strsplit(body[[1]], split = "\n")
    } else {
      body_lines <- c("")
    }
    returned_string <- header_lines[[1]][ charmatch("HTTP", header_lines[[1]]) ]
    #PUT 
  }


  # Lots of values including time, size, etc in getCurlInfo...
  returned_code <-  getCurlInfo(curl_handle)$response.code
  res <- list(statusCode = returned_code, headers = header_lines)
  bunyanLog.debug(msg ="mantaXfer server return", res = res, version = manta_globals$version) 

# Server Error Responses
  if (as.integer(returned_code) >= 300) {
    if (curl_method == "GET") {  # convert buffer to text to read body
       if (length(b) == 0) {
         body_lines <- c("")
       } else {
         body_lines = strsplit(rawToChar(b),split = "\n")
       }
    }
    msg <- ""
    if (isValidJSON(body_lines[[1]], asText = TRUE)) {
      values <- fromJSON(body_lines[[1]])
      # this checks the error strings to see if it is on the list...
      if (sum(charmatch(values, manta_globals$manta_error_classes, nomatch = 0)) > 0) {
        msg <- paste("Manta Service Error: ", returned_code, "\n", sep="")
      } else {
        msg <- paste("mantaXfer Unknown Error: ", returned_code, "\n", sep="")
      }
      # It was valid JSON, so show it as the return error message
      values <- paste(values, collapse = "\n") # make one string
      msg <- paste(msg, values, "\n", sep="")
    } else {
      # not valid JSON returned, just return the error code...
      msg <- paste("mantaXfer Unrecognized - Server Error Code: ", returned_code, returned_string, "\n", sep=" ")
    }
    bunyanLog.error(msg = msg, version = manta_globals$version)
    cat(msg)
    return(FALSE)
  }

  if (as.integer(returned_code) != 0)  {
    if (as.integer(returned_code) == returncode) {
      if ((returnbuffer == TRUE) && (curl_method == "GET")) {
        if (returnmetadata == TRUE) {
          return(list(metadata = header_lines, buffer = b))
        } else {
          return(b)
        }
      }
      if (curl_method == "GET") {
        f = file(filename, "wb")
        writeBin(con = f, object = b)
        flush(f)
        close(f)
        if (returnmetadata == TRUE) {
          return(header_lines)
        }
      }
      return(TRUE)
    } 
  } else { 
   # some other 0 - 300 numbered message that isn't a matching return code 
    msg <- paste("mantaXfer - Unrecognized Server Code:", returned_string, "\n", sep=" ")
    bunyanLog.error(msg = msg, version = manta_globals$version)
    cat(msg)
    return(FALSE)
  }
}
