# Roxygen Comments mantaPut
#' Uploads file(s) (vectorized), or raw R buffer data to Manta Storage Service. 
#'
#' Transfers file, buffer to Manta Storage Object specified in mantapath.
#' To save a file, specify the filename. It will go into the current working Manta
#' directory with the same name unless absolute Manta path or relative object name 
#' is specified with the \code{mantapath} argument. To see the current directory on Manta
#' use \code{\link{mantaGetwd}}, and \code{\link{mantaSetwd}} to change it. 
#'
#' To save a raw R buffer, pass the string with its name e.g. buffer = "myRawBuffer"
#'
#' One limitation of the \code{mantaRSDK} is that it is not designed to handle large 
#' (multi-Gigabyte or larger) objects. Uploads - \code{mantaPut} - work from files, 
#' but Downloads - \code{\link{mantaGet}} - fill the R memory space to their completion 
#' before being written to a file. To download files larger than your R memory, 
#' use the Manta Node.js command line tool \code{mget}. The Node.js based Manta command  
#' line tools employ streams, so object size is not a limitation.
#'
#' The Content-Type information metatdata for the file is obtained using the 
#' RCurl library function \code{guessMIMEtype} on the filename extension provided, 
#' e.g. \code{.jpg} or it can be set 
#' by passing a \code{header} parameter, which is an \code{RCurl} style HTTP header 
#' - a list of named  character values like this:\cr 
#' \code{header = c('content-type' = "image/jpg")}. The default
#' \code{Content-Type} header is \code{"application/octet-stream"}.  The number of
#' copies (durability level) saved is by default 2. This can be changed by using 
#' headers like this: \cr
#' \code{headers = c('x-durability-level' = 4)} for one-time use.\cr 
#' For larger operations use \code{\link{mantaSetLimits}} to change the default 
#' durability level for the current mantaRSDK session. The
#' number of copies stored can range from 2 to 6. This function does not support
#' streaming uploads, for that use the Node.js Manta command line interface (CLI). 
#' Other Manta operations involving HTTP conditional request semantics and CORS headers 
#' are not implemented in this R library, but can be used with the Node.js CLI 
#' which can be called from R with the \code{system2} command shell.
#' \code{mantaPut} is a wrapper for \code{mantaXfer}, which implements the 
#' actual \code{RCURL} data upload.
#'
#' @param mantapath character, optional. Path to where uploaded data will go on 
#' Manta or Manta Object file name in current working Manta directory. 
#' If \code{mantapath} ends in "/"  it is assumed to be 
#' specifying a Manta subdirectory and the filename portion is appended to it.
#' Memory data uploads using \code{buffer} parameter require \code{mantapath} to have
#' a destination file name at the end of the path with an extension for proper
#' guessing of \code{Content-Type} header information.
#' 
#' @param filename vector of character, optional. Path to local file to upload. 
#' If only a filename is given, 
#' assumes file is in path specified by \code{getwd}. Vectorized.
#'
#' @param buffer optional. Raw buffer of data to put. If filename is specified, buffer is 
#' ignored and filename contents are uploaded. Not vectorized.
#'
#' @param md5 logical optional. Test md5 hash of file/raw buffer with OpenSSL 
#' before/after upload. \code{TRUE} by default, setting \code{FALSE} will speed up
#' transfers a bit.
#'
#' @param headers optional. Headers including R structured metadata (up to 4k in user metadata)
#' as array of named character 
#' E.g.\cr 
#' \code{headers = c('content-type' = "image/jpg", 'x-durability-level' = 4)}
#' Manta user metadata is prefixed with \code{"m-"}, E.g.\cr
#' \code{headers = c('content-type' = "x-chemical/x-pdb", 'm-molecule-class' = "protein")}
#' 
#' @param info logical required.  \code{FALSE} silences output messages while downloading.
#'
#' @param verbose logical, optional. Passed to \code{RCurl GetURL},
#' Set to \code{TRUE} to see background REST communication on \code{stderr}
#' Note this is not visible on Windows.
#' 
#' @return \code{TRUE} or \code{FALSE} depending on success of upload.
#'
#' @family mantaPut
#'
#' @seealso \code{\link{mantaGet}}
#'
#' @keywords Manta, manta
#'
#' @examples
#' \dontrun{
#' ## Save a static hello world HTML page
#' htmlpage <- paste("<!DOCTYPE html>\n<html>\n<body>\n\n",
#'                   "<h1>Hello from Joyent Manta.</h1>\n\n",
#'                   "<p>Hello world! from ",
#'                   mantaWhoami(),
#'                   ".</p>\n\n",
#'                   "</body>\n</html>", sep="")
#' file <- file("test_index.html", "wb")
#' write(htmlpage,file)
#' close(file)
#' rm(file)
#' rm(htmlpage)
#'
#' ## Upload the HTML file to Manta in your public area
#' mantaSetwd.public()
#' mantaPut("test_index.html")
#' mantaExists("test_index.html")
#' mantaCat("test_index.html")
#'
#' ls()
#' buffer <- mantaGet("test_index.html", buffer = TRUE)
#' cat(rawToChar(buffer))
#' 
#' ## Upload the raw buffer
#' mantaPut(mantapath = "~~/public/buffer_index.html", buffer = buffer)
#' mantaLs.l(grepfor = "[.]html")
#' mantaCat("buffer_index.html")
#'
#' ## Check file metadata to see Content-type
#' mantaGet("test_index.html", metadata = TRUE)
#' mantaGet("buffer_index.html", metadata = TRUE)
#'
#' ## copy and paste URL into browser.
#' mantaLs.url("~~/public", grepfor = "[.]html")
#'
#' ## Cleanup this demo
#' #mantaRm("~~/public/test_index.html")
#' #mantaRm("~~/public/buffer_index.html")
#' #file.remove("test_index.html")
#' #rm(buffer)
#' }
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
          pathsplit <- strsplit(filename,"[\\]")
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
           pathsplit <- strsplit(filename,"[\\]")
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
      pathsplit <- strsplit(filename,"[\\]")
   }
   test_filename <- pathsplit[[1]][length(pathsplit[[1]])]
  } else {   
    # No file, so guess content-header from mantapath destination filename
    pathsplit <- strsplit(mantapath, "/")
    test_filename <- pathsplit[[1]][length(pathsplit[[1]])]    
  }
  mimetype <- guessMIMEType(test_filename, default = "application/octet-stream")
  put_headers <- c('content-type' = as.character(mimetype))
  # bug in guessMIMEType that leaves mimeTypeExtensions data in workspace...
  rm(mimeTypeExtensions, envir = .GlobalEnv)

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
     msg <- paste("Uploading R raw memory buffer to Manta: ", mantapath, "\n", " as Content-Type: ", mimetype, "\n", sep="")
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




