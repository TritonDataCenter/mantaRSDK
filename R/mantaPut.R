## TODO BUFFER AND OBJECT HANDLING WHEN FILENAME IS MISSING NOT THIS
  # durability-level (range from 1-6) - USE mantaSetLimits
  # Content-Type  (default application/octet-stream)
  # HTTP conditional request semantics If-Match or If-Modified-Since
  # CORS headers are saved as appropriate - Preflighted rquests
  # are supported by sending a list of values in
  # access-control-allow-origin
  # Default object size limit is 5Gb without override
  # max-content-length is a guess of how big the object is when using
  # transfer-encoding chunked
  # Non streaming - set content-length header
  # tags for metadata are headers prefixed with m-
  # e.g. m-local-user foo (up to 4Kb of header metadata)
  #  manta_headers <- c('x-durability-level' = manta_globals$durability_level , headers)
  #  'Content-Length' = length of content
  #  'Content-MD5' .. call openssl to compute that
  #  'max-content-length' ... object size  are we using transfer-encoding-chunked?
  #  'm-local-user'
# Roxygen Comments mantaPut
#'
#' mantaPut Transfers file, object or buffer to the object specified in mantapath
#'
#' @param mantapath string, optional. Path to a manta object or object name in current
#' working Manta directory. If manapath ends in "/" or "\" it is assumed to be 
#' specifying a Manta subdirectoy and the filename portion is appended to it.
#' 
#' @param filename optional. Path to local file to PUT. If only a filename is given, 
#' assumes file is in path specified by getwd(). 
#'
#' @param buffer optional. Raw buffer of data to put.
#'
#' @param object optional. Name of R object to PUT 
#'
#' @param md5 logical optional. Test md5 hash of data before/after PUT transfer
#'
#' @param envir optional. Environment of R object
#'
#' @param headers optional. Headers including R structured metadata (up to 4k in user metadata)
#' as array of named strings
#'
#' @param verbose logical, optional. Passed to RCurl GetURL,
#' Set to TRUE to see background REST communication on stderr
#' which is invisible on Windows
#' 
#' @return TRUE or FALSE depending on success of PUT transfer
#'
#' @keywords Manta, manta
#'
#' @export
mantaPut <-
function(filename, mantapath, buffer, object, md5 = FALSE, envir, headers, verbose = FALSE) {

  if ( missing(mantapath) && missing(filename) ) {
         stop("mantaPut - No Manta object specified, no local file specified.")
  } else {
     if ( missing(mantapath) && (!missing(filename)) ) {
        # filename exists - ignoring any values in buffer, object
        # Extract the trailing filename string as the source name for mantapath
        #
        pathsplit <- strsplit(filename,"/")
        manta_filename <- pathsplit[[1]][length(pathsplit[[1]])]
        mantapath <- paste(mantaGetwd(), "/", manta_filename, sep = "")
        path_enc <- mantaExpandPath(mantapath)
        object <- NA
        buffer <- NA
     } else {
        if ( missing(filename) && (!missing(mantapath)) ) {  
           # mantapath supplied - assume filename is the same to go to getwd()
## TODO BUFFER AND OBJECT HANDLING WHEN FILENAME IS MISSING NOT THIS
           path_enc <- mantaExpandPath(mantapath)
           if (path_enc == "") {
             mantapath <- paste(mantaGetwd(), "/", mantapath, sep = "")
           }
        
        } else {
           if ( (!missing(filename)) && (!missing(mantapath)) ) { 
             if ( substr(mantapath, nchar(mantapath), nchar(mantapath)) == "/" ) {
               # put assumes the file is local and if mantapath ends with / - reuse the local filename
               pathsplit <- strsplit(filename,"/")
               from_filename <- pathsplit[[1]][length(pathsplit[[1]])]
               path_enc <- mantaExpandPath(mantapath) # is mantapath valid?
               if (path_enc == "") {  # nope,  assume path from working directory
                  mantapath <- paste(mantaGetwd(), "/", mantapath, sep = "")
               }
               mantapath <- paste(mantapath, from_filename, sep="")
             } # else just assume mantapath is full filespec
             path_enc <- mantaExpandPath(mantapath)
             object <- NA
             buffer <- NA
           }
        }
     }
  }

#
#  metadata header construction functions...  
#  headers <- c('content-type' = "")
#  Must check and specify Content-Type... esp for images, ...



# Content-Type  (default application/octet-stream)

  if ((path_enc == "") || (filename == "")) {
    msg <- paste("mantaPut - Missing argument error - mantapath: [", mantapath, "] or filename: [", filename,  "]\n", sep = "")
    bunyanLog.error(msg)
    stop(msg)
  }


  pathsplit <- strsplit(filename,"/")
  test_filename <- pathsplit[[1]][length(pathsplit[[1]])]
  mimetype <- guessMIMEType(test_filename, default = "application/octet-stream")
  headers <- c('content-type' = as.character(mimetype))


return( mantaXfer(action = path_enc, method = "PUT", filename = filename, buffer = buffer, 
         object = object, md5 = md5, envir = envir, headers = headers, verbose = verbose) )

}




