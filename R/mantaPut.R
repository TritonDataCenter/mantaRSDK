# Roxygen Comments mantaPut
#' Uploads file(s) (vectorized), or raw R buffer data to Manta Storage Service. 
#'
#' Transfers file, buffer to Manta Storage Object file specified in mantapath.
#'
#' To save a file, specify the filename. It will go into the current working Manta
#' directory with the same name unless a Manta path or file name is specified with 
#' the mantapath argument. To see the  current directory on Manta
#' use mantaGetwd(), and mantaSetwd() to change it. 
#'
#' To save a raw R buffer, pass the string with its name e.g. buffer = "myRawBuffer"
#'
#' One limitation of the mantaRSDK is that it is not designed to handle large 
#' (multi-Gigabyte or larger) objects. Uploads - mantaPut() - work from files, 
#' but Downloads - mantaGet() - fill the R memory space to their completion 
#' before being written to a file. To download files larger than your R memory, 
#' use the Manta Node.js command line tool mget. The Node.js based Manta command  
#' line tools employ streams, so object size is not a limitation.
#'
#' The Content-Type information metatdata for the file is obtained using the 
#' RCurl library function guessMIMEtype on the filename.ext provided, or it can be set 
#' by passing a header parameter, which is an RCurl style HTTP header list of named 
#' character values like this: header = c('content-type' = "image/jpg"). The default
#' Content-Type header is 'application/octet-stream'.  The number of
#' copies saved is by default 2. This can be changed by using  headers like this:
#' headers = c('x-durability-level' = 4) for one-time use, or for larger operations
#' use mantaSetLimits to change the default for the current mantaRSDK session. The
#' number of copies stored can range from 2 to 6. This function does not support
#' streaming uploads, for that use the Node.js Manta command line interface (CLI). 
#' Other Manta operations involving HTTP conditional request semantics and CORS headers 
#' are not implemented in this R library, but can be used with the Node.js CLI 
#' which can be called from R with the system2() command shell.
#'
#' mantaPut is a wrapper for mantaXfer, which implements the RCURL transfer
#'
#' @param mantapath string, optional. Path to where uploaded data will go on 
#' Manta or Manta Object file name in current working Manta directory. 
#' If mantapath ends in "/"  it is assumed to be 
#' specifying a Manta subdirectory and the filename portion is appended to it.
#' Memory data uploads using buffer parameter require mantapath to have
#' a destination file name at the end of the path with an extension for proper
#' guessing of Content-Type header information.
#' 
#' @param filename vector of character, optional. Path to local file to PUT. If only a filename is given, 
#' assumes file is in path specified by getwd(). Vectorized.
#'
#' @param buffer optional. Raw buffer of data to put. If filename is specified, buffer is 
#' ignored and filename contents are uploaded. Not vectorized.
#'
#' @param md5 logical optional. Test md5 hash of file/raw buffer before/after PUT transfer. 
#'
#' @param headers optional. Headers including R structured metadata (up to 4k in user metadata)
#' as array of named character 
#' E.g. headers = c('content-type' = "image/jpg", 'x-durability-level' = 4)
#' Manta user metadata is prefixed with "m-", E.g.
#' headers = c('content-type' = "x-chemical/x-pdb", 'm-molecule-class' = "protein")
#' 
#' @param info logical required. Set to FALSE to silence output messages while downloading.
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
function(filename, mantapath, buffer, md5 = FALSE, headers, info = TRUE, verbose = FALSE) {


  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }


  if ( missing(filename) && missing(buffer) ) {
    stop("mantaPut, No file, or R raw buffer specified - missing values for filename or buffer arguments.")
  }

  if (!missing(filename)) {
    if (length(filename) > 1) {
      if (info == TRUE) {
       cat(paste("Uploading ", length(filename), " files.\n"))
      }
      if (missing(headers)) {
        return(unlist(lapply(filename, mantaPut, md5 = md5, info = info, verbose = verbose)))
      } else {
        return(unlist(lapply(filename, mantaPut, md5 = md5, info = info, headers = headers, verbose = verbose)))
      }
    }
  }

  memorydata <- FALSE
  if (missing(filename) ) {
     if (!missing(buffer)) {
       if (missing(mantapath)) {
          stop("mantaPut. No name provided to store R raw memory data buffer on Manta - use mantapath parameter")
       } else {
         if ( substr(mantapath, nchar(mantapath), nchar(mantapath)) == "/" ) {
           stop("mantaPut. Cannot resolve target file name to store R raw buffer on Manta - mantapath ends in /")
         }
       }
       memorydata <- TRUE
       filename <- ""
     }
  }

  if ( (memorydata == FALSE) && missing(mantapath) && (filename == "") ) {
         stop("mantaPut - No Manta Storage Object specified, no local file specified.")
  } 

  if ( memorydata == FALSE ) {  # filename is not missing
     if ( missing(mantapath) ) {
        # Extract the trailing filename string as the destination name for mantapath
        if (.Platform$OS.type == "unix") {
          pathsplit <- strsplit(filename,"/")
        } else {
          pathsplit <- strsplit(filename,"\\")
        }
        manta_filename <- pathsplit[[1]][length(pathsplit[[1]])]
        mantapath <- paste(mantaGetwd(), "/", manta_filename, sep = "")
        path_enc <- mantaExpandPath(mantapath)
     } else { # we have mantapath
       if ( substr(mantapath, nchar(mantapath), nchar(mantapath)) == "/" ) {
         # put assumes the file is local and if mantapath ends with / - reuse the local filename
         if (.Platform$OS.type == "unix") {
           pathsplit <- strsplit(filename,"/")
         } else {
           pathsplit <- strsplit(filename,"\\")
         }
         from_filename <- pathsplit[[1]][length(pathsplit[[1]])]
         path_enc <- mantaExpandPath(mantapath) # is mantapath valid?
         if (path_enc == "") {  # nope,  assume path from working directory
            mantapath <- paste(mantaGetwd(), "/", mantapath, sep = "")
         }
         mantapath <- paste(mantapath, from_filename, sep="")
       } # skipped -  just assume mantapath supplied is full filespec, try it
       path_enc <- mantaExpandPath(mantapath)
     }
     buffer <- NA
  } else {
    # we need to find the mantapath provided  destination file name to figure out
    # a mime-type guess 
    pathsplit <- strsplit(mantapath,"/")
    to_filename <- pathsplit[[1]][length(pathsplit[[1]])]
    path_enc <- mantaExpandPath(mantapath) # is mantapath valid?
    if (path_enc == "") {  # nope,  assume path from working directory
      mantapath <- paste(mantaGetwd(), "/", mantapath, sep = "")
      path_enc <- mantaExpandPath(mantapath)
    }
  }


  
  if ( ( (path_enc == "") || (filename == "") ) && (memorydata == FALSE) )  {
    msg <- paste("mantaPut - Argument error - mantapath: [", mantapath, "] or filename: [", filename,  "]\n", sep = "")
    bunyanLog.error(msg)
    stop(msg)
  }

  if (path_enc == "") {
    msg <- paste("mantaPut - Argument error - mantapath: [", mantapath, "]\n", sep = "")
    bunyanLog.error(msg)
    stop(msg)
  } 

  ## MIME Content-Type settings from filename.ext 
  test_filename <- ""
  if (memorydata == FALSE) {  
   # Guess content-type header from filename source  
   if (.Platform$OS.type == "unix") {
      pathsplit <- strsplit(filename,"/")
   } else {
      pathsplit <- strsplit(filename,"\\")
   }
   test_filename <- pathsplit[[1]][length(pathsplit[[1]])]
  } else {   
    # No file, so guess content-header from mantapath destination filename
    pathsplit <- strsplit(mantapath, "/")
    test_filename <- pathsplit[[1]][length(pathsplit[[1]])]    
  }
  mimetype <- guessMIMEType(test_filename, default = "application/octet-stream")
  put_headers <- c('content-type' = as.character(mimetype))

  if (!missing(headers)) {
    ## See if user header overrides content-type guess...
  }


  # Set durability-level header
  if (manta_globals$manta_defaults$durability_level != 2) {
     put_headers <- c(put_headers, 'x-durability-level' = manta_globals$manta_defaults$durability_level)
  }

  # Append user-headers  
  if (!missing(headers)) {
    put_headers <- c(put_headers, headers)
  }

  if (memorydata == TRUE) {
     msg <- paste("Uploading R raw memory buffer to Manta: ", mantapath, "\n", " as Content-Type: ", mimetype, sep="")
  } else {
     msg <- paste("Uploading file: ", filename, " to Manta: ", mantapath, "\n", sep="")
  }
  bunyanLog.debug(msg)
  if (info == TRUE) {
    cat(msg)
    cat("Working...")
  }

  returnval <-  mantaXfer(action = path_enc, method = "PUT", filename = filename, buffer = buffer, 
          md5 = md5, headers = put_headers, verbose = verbose) 

  if (info == TRUE) {
    if (returnval == TRUE) {
      cat("..Done\n")
    } else {
      cat("..Failed\n")
    }
  }

  return(returnval)

}




