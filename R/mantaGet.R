# Roxygen Comments mantaGet
#'
#' Downloads Manta object(s) (vectorized) specified to file(s) or buffer.
#'
#' One limitation of the mantaRSDK is that it is not designed to handle large 
#' (multi-Gigabyte or larger) objects. Uploads - mantaPut() - work from files, 
#' but Downloads - mantaGet() - fill the R memory space to their completion 
#' before being written to a file. To download files larger than your R memory, 
#' use the Manta Node.js command line tool mget. The Node.js based Manta command  
#' line tools employ streams, so object size is not a limitation.
#'
#' @param mantapath vector of character, optional. Path to a manta object or object name in current
#' working Manta directory for retrieval. Vectorized, e.g. mantaGet(mantaLs.paths(items = 'o')) will
#' download the contents of the Manta working directory to your local working R directory, and 
#' mantaGet(mantaLs.paths(items = 'o'), metadata = TRUE) will
#' download and return just the metadata in R format for the Manta working directory contents.
#'
#' @param filename optional. Assumes this is the filename in the current path.
#' Downloads file to the local path specified by getwd() if no path specified. If not 
#' provided downloads to a file with same name as Manta object.
#'
#' @param buffer logical required. When TRUE return a buffer with data. Not supported
#' for vectorized mantapath input.
#' 
#' @param metadata logical optional. Set TRUE to Retrieve R metadata in return value.
#'
#' @param info logical. Set to FALSE to suppress Downloading message.
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
function(mantapath, filename,  buffer = FALSE, metadata = FALSE, info = TRUE, verbose = FALSE) {


  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  if (!missing(mantapath)) {
    if (length(mantapath) > 1) {
      if (info == TRUE) {
       cat(paste("Downloading ", length(mantapath), " files.\n"))
      }
      if (metadata == FALSE) {
        return(unlist(lapply(mantapath, mantaGet, metadata = metadata, info = info, verbose = verbose)))
      } else {
        return(lapply(mantapath, mantaGet, metadata = metadata, info = info, verbose = verbose))
      }
    }
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
  if (metadata == FALSE) {
    msg <- paste("Downloading Manta object: ", mantapath, " To ", filename, "\n", sep="")
  } else {
    msg <- paste("Downloading Manta object metadata: ", mantapath, "\n", sep="")
  }
  bunyanLog.info(msg)
  if (info == TRUE) {
    cat(msg)
  }
  if (metadata == FALSE) {
    return( mantaXfer(action  = path_enc, method = "GET", filename = filename, returnmetadata = metadata, 
        returnbuffer = buffer,  verbose = verbose) )
  } else {
    return(list(mantapath = mantapath, metadata =  mantaXfer(action  = path_enc, method = "HEAD", filename = filename, returnmetadata = metadata,
        returnbuffer = buffer,  verbose = verbose) ))
  }

}


