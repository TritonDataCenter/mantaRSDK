# Roxygen Comments mantaSave.image
#' Workspace Upload function that calls R save.image(); used by mantaSave.ws().
#'
#' mantaSave.image uses mantaSave,  mantaXfer, which implements the RCURL transfer
#'
#' @param mantapath optional. Path/filename to where uploaded data will go on 
#' Manta or Manta object/file name in current working Manta directory. If no 
#' extension is provided on the filename, or a non R data style extension 
#' ".rda" is appended to the end of the filename.  
#' 
#' @param md5 logical. Test md5 hash of R data tempfile before/after PUT transfer.
#'
#' @param headers optional. Headers for HTTP transfer, in RCurl style. See mantaPut()
#' User metadata headers may be provided, E.g.:
#  headers = c('m-Title' = "Model Fitting Test", 'm-Iteration' = "42")
#' Avoid supplying the content-type header, which is set to the R data type 
#' "application/x-r-data", and the durability-level header which is handled 
#' via the durability parameter. 
#'
#' @param durability optional. Number of copies to store on Manta (2-6). If not
#' provided, uses saved value from mantaSetLimits(), system default is 2.
#'
#' @param ascii optional. See save().
#'
#' @param version optional. See save().
#'
#' @param compress optional. See save().
#'
#' @param info logical required. Set to FALSE to silence output messages while downloading.
#'
#' @param verbose logical, optional. Passed to RCurl GetURL,
#' Set to TRUE to see background REST communication on stderr
#' which is invisible on Windows
#' 
#' @return TRUE or FALSE depending on success of transfer
#'
#' @keywords Manta, manta
#'
#' @export
mantaSave.image <-
function(mantapath, md5 = TRUE, headers, durability = 2, version = NULL, 
         ascii = FALSE,  compress = !ascii, info = TRUE, verbose = FALSE) {


  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }


    opts <- getOption("save.image.defaults")
    if (is.null(opts)) 
        opts <- getOption("save.defaults")
    if (missing(ascii) && !is.null(opts$ascii)) 
        ascii <- opts$ascii
    if (missing(compress) && !is.null(opts$compress)) 
        compress <- opts$compress
    if (missing(version)) 
        version <- opts$version

  if ( substr(mantapath, nchar(mantapath), nchar(mantapath)) == "/" ) {   
    stop("mantaSave.image. Cannot resolve target file name to store R memory object on Manta - mantapath ends in /")
  }

  temp_f <- tempfile()

  save(list = ls(envir = .GlobalEnv, all.names = TRUE), file = temp_f,  
        version = version, ascii = ascii, compress = compress, 
        envir = .GlobalEnv, precheck = FALSE)

  # File extensions conforming to RData type 
  pathsplit <- strsplit(mantapath,"/")
  to_filename <- pathsplit[[1]][length(pathsplit[[1]])]
  extensions <- c( "rda", "RData", "Rdata", "rdata") 
  ## does filename have extension, if not append one
  namesplit <- strsplit(to_filename,"[.]")
  if (length(namesplit[[1]]) > 1) { 
    # extension present
    # case where supplied is ".Rdata" gives a count of 2, so this is ok - 
    ext <- namesplit[[1]][length(namesplit[[1]])]
    if (is.na(match(ext, extensions))) { 
      # Extension does not match, append another valid extension
      # so "myimage.now" becomes "myimage.now.RData"
      mantapath <- paste(mantapath, ".RData", sep = "")  
    }
  } else { 
    # no extension supplied, append extension
    # so "myfile" becomes "myfile.RData"
    mantapath <- paste(mantapath, ".Rdata", sep = "")
  }


  path_enc <- mantaExpandPath(mantapath) # is mantapath a valid mantapath?
  if (path_enc == "") {  # nope,  assume path from working directory
    mantapath <- paste(mantaGetwd(), "/", mantapath, sep = "")
    path_enc <- mantaExpandPath(mantapath) 
  }

  if (path_enc == "") {
    msg <- paste("mantaSave.image - Argument error - mantapath: [", mantapath, "]\n", sep = "")
    bunyanLog.error(msg)
    stop(msg)
  } 

  if (missing(durability)) {
   durability <- manta_globals$manta_defaults$durability_level
  } else {
    if (durability > 6) durability <- 6
    if (durability < 2) durability <- 2
  }

  content_header <- c('content-type' = "application/x-r-data")
  # Set durability-level header if not Manta default value of 2
  if (durability != 2) {
     put_headers <- c(content_header, 'x-durability-level' = durability)
  } else {
     put_headers <- content_header
  }

  # Append user-headers  
  if (!missing(headers)) {
    put_headers <- c(put_headers, headers)
  }

  msg <- paste("Uploading R memory data to Manta object: ", mantapath, "\n",  sep="")
  bunyanLog.debug(msg)
  if (info == TRUE) {
    cat(msg)
    cat("Working...\n")
  }

  returnval <-  mantaXfer(action = path_enc, method = "PUT", filename = temp_f, 
          md5 = md5, headers = put_headers, verbose = verbose) 

  if (info == TRUE) {
    if (returnval == TRUE) {
      cat("..Done\n")
    } else {
      cat("..Failed\n")
    }
  }

  file.remove(temp_f)
  return(returnval)

}

