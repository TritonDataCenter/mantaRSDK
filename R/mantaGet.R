## TODO
## parse the metadata into something useful by user
## HEADERS
## USER METADATA
## TEST WINDOWS PATHS
# Roxygen Comments mantaGet
#'
#' mantaGet Transfers object specified in mantapath to file or buffer
#'
#' @param mantapath string, optional. Path to a manta object or object name in current
#' working Manta directory for retrieval. 
#'
#' @param filename optional. Assumes this is the filename in the current path.
#' Downloads file to the local path specified by getwd() if no path specified. If not 
#' provided downloads to a file with same name as Manta object.
#'
#' @param buffer logical required. When TRUE return a buffer with data.
#' 
#' @param metadata logical optional. Set TRUE to Retrieve R metadata in return value.
#'
#' @param verbose logical, optional. Passed to RCurl GetURL,
#' Set to TRUE to see background REST communication on stderr
#' which is invisible on Windows
#' 
#' @return TRUE or FALSE depending on success of GET transfer
#'
#' @keywords Manta, manta
#'
#' @export
mantaGet <-
function(mantapath, filename,  buffer = FALSE, metadata = FALSE, verbose = FALSE) {


  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }


  if ( missing(mantapath) && missing(filename) ) {
         stop("mantaGet - No Manta object specified, no local file specified.")
  } else {
     if ( missing(mantapath) && (!missing(filename)) ) {
        # filename exists - ignoring any values in buffer, object
        # Extract the trailing filename string as the source name for mantapath
        #
        pathsplit <- strsplit(filename,"/")
        manta_filename <- pathsplit[[1]][length(pathsplit[[1]])]
        mantapath <- paste(mantaGetwd(), "/", manta_filename, sep = "")
        path_enc <- mantaExpandPath(mantapath)
     } else {
        if ( missing(filename) && (!missing(mantapath)) ) {  
           # mantapath supplied - assume filename is the same to go to getwd()
           pathsplit <- strsplit(mantapath,"/")
           filename <- pathsplit[[1]][length(pathsplit[[1]])]
           path_enc <- mantaExpandPath(mantapath)
           if (path_enc == "") {
             mantapath <- paste(mantaGetwd(), "/", mantapath, sep = "")
           }
           path_enc <- mantaExpandPath(mantapath)
        } else {
           if ( (!missing(filename)) && (!missing(mantapath)) ) { 
##TODO - THIS IS NOT WINDOWS READY - terminating \
              if ( substr(filename, nchar(filename), nchar(filename)) == "/" ) {
                # Get assumes the file is remote and if filename ends with / or \ 
                # - it is a path, reuse the remote filename
                pathsplit <- strsplit(mantapath,"/")
                manta_filename <- pathsplit[[1]][length(pathsplit[[1]])]
                filename <- paste(filename, manta_filename, sep="")
              } 
              path_enc <- mantaExpandPath(mantapath)  # first see if the path is ok
              if (path_enc == "") {  # assume working directory
                mantapath <- paste(mantaGetwd(), "/", mantapath, sep = "")
              }
              path_enc <- mantaExpandPath(mantapath) 
           }
        }
     }
  }

  if (path_enc == "") {
    msg <- paste("mantaGet - Cannot resolve mantapath:", mantapath, "\n", sep = "")
    bunyanLog.error(msg)
    stop(msg)
  }
  return( mantaXfer(action  = path_enc, method = "GET", filename = filename, returnmetadata = metadata, 
        returnbuffer = buffer,  verbose = verbose) )

}


