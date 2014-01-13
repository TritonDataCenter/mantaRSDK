# Roxygen Comments mantaSource
#' 
#' Downloads specified Manta object and applies source() to parse R code file.
#'
#' @param mantapath string, optional. Path to a manta R code file or file  name in current
#' working Manta directory for retrieval. 
#'
#' @param local logical optional. See source().
#'
#' @param verbose logical, optional. Passed to RCurl GetURL,
#' Set to TRUE to see background REST communication on stderr
#' which is invisible on Windows
#' 
#' @param max.deparse.length optional. See source().
#'
#' @param encoding optional. See source().
#'
#' @param keep.source optional. See source().
#'
#' @return TRUE or FALSE depending on success of GET transfer
#'
#' @keywords Manta, manta
#'
#' @export
mantaSource <-
function(mantapath, local = FALSE, verbose = FALSE, max.deparse.length = 150, 
         encoding = getOption("encoding"), keep.source = getOption("keep.source") ) {


  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }


  if ( missing(mantapath) ) {
    stop("mantaSource - No Manta Storage Object specified - use mantapath parameter.")
  } else {
    ## check for / at end of mantapath
    if (  substr(mantapath, nchar(mantapath), nchar(mantapath)) == "/"  ) {
       stop("mantaSource - No Manta Storage Object specified, mantapath implies a subdirectory.")
    }
    pathsplit <- strsplit(mantapath,"/")
    f_name <- pathsplit[[1]][length(pathsplit[[1]])]
    # mantapath supplied - does it end in .R or other correct extension?
    # If not, don't source.
    extensions <- c( "r", "R", "s", "S", "q", "Q")
    ## does filename have extension, if not append one
    namesplit <- strsplit(f_name,"[.]")
    if (length(namesplit[[1]]) > 1) { #extension present
      # case where supplied is ".R" gives a count of 2, so this is ok -  
      ext <- namesplit[[1]][length(namesplit[[1]])]
      if (is.na(match(ext, extensions))) {
        # Extension does not match, append another valid extension
        # so "myfile.csv" becomes "myfile.csv.R"
        mantapath <- paste(mantapath, ".R", sep = "")
      }  
    } else {
      # no extension supplied, append extension
      # so "myfile" becomes "myfile.R"
      mantapath <- paste(mantapath, ".R", sep = "")
    }
    path_enc <- mantaExpandPath(mantapath)
    if (path_enc == "") {
      mantapath <- paste(mantaGetwd(), "/", mantapath, sep = "")
    }
    path_enc <- mantaExpandPath(mantapath)
  }

  if (path_enc == "") {
    msg <- paste("mantaSource - Cannot resolve mantapath:", mantapath, "\n", sep = "")
    bunyanLog.error(msg)
    stop(msg)
  }

  if (mantaExists(curlUnescape(path_enc)) == FALSE) {
    msg <- paste("mantaSource - Manta object not found:", mantapath, "\n", sep = "")
    bunyanLog.error(msg)
    stop(msg)
  }


  filename <- tempfile()
  retval <- mantaXfer(action  = path_enc, method = "GET", filename = filename, returnmetadata = TRUE, 
        returnbuffer = FALSE,  verbose = verbose)

  ### Look at the returned metadata for proper content-type
  ## application/x-r-data

  type <-  retval[[1]][grepl("content-type:", retval[[1]], ignore.case = TRUE)]
  mimetype <- "text/R-code"

  if ( length( grep(mimetype, type, ignore.case = TRUE) ) == 1 ) {
    source(filename, local = local, verbose = verbose, max.deparse.length = max.deparse.length, 
             encoding = encoding,  keep.source = keep.source) 
    file.remove(filename)
    return(TRUE)
  } else {
    msg <- paste("mantaSource metadata mismatch, expecting:", mimetype, " received: " , type, sep="")
    bunyanLog.error(msg)
    cat(msg)
    file.remove(filename)
    return(FALSE)
  }
}


