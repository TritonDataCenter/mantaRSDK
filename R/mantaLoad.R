# Roxygen Comments mantaLoad
#'
#' Loads R data object specified in mantapath.
#'
#' @param mantapath string, optional. Path to a manta R data file or file name in current
#' working Manta directory for retrieval. 
#'
#' @param envir optional. Environment in which to load, See load().
#'
#' @param info optional. Print information messages.
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
mantaLoad <-
function(mantapath, envir = parent.frame(), info = TRUE, verbose = FALSE) {


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
    # mantapath supplied - does it end in .rda or .RData or other correct extension?
    # If not, don't source.
    extensions <- c( "rda", "RData", "Rdata", "rdata")
    ## does filename have extension, if not append one
    namesplit <- strsplit(f_name,"[.]")
    if (length(namesplit[[1]]) > 1) { #extension present
      # case where supplied is ".RData" gives a count of 2, so this is ok -
      ext <- namesplit[[1]][length(namesplit[[1]])]
      if (is.na(match(ext, extensions))) {
        # Extension does not match, append another valid extension
        # so "myfile.csv" becomes "myfile.csv.rda"
        mantapath <- paste(mantapath, ".rda", sep = "")
      }
    } else {
      # no extension supplied, append extension
      # so "myfile" becomes "myfile.rda"
      mantapath <- paste(mantapath, ".rda", sep = "")
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


  filename <- tempfile()
  retval <- mantaXfer(action  = path_enc, method = "GET", filename = filename, returnmetadata = TRUE, 
        returnbuffer = FALSE,  verbose = verbose) 


  ### Look at the returned metadata for proper content-type 
  ## application/x-r-data   

  type <-  retval[[1]][grepl("content-type:",  retval[[1]], ignore.case = TRUE)]
  mimetype <- "application/x-r-data"

  if ( length( grep(mimetype, type, ignore.case = TRUE)) == 1 ) {
    load(filename, envir = envir, verbose = verbose)
    file.remove(filename) 
    return(TRUE)
  } else {
    msg <- paste("mantaLoad metadata mismatch, expecting:", mimetype, " received: " , type, sep="")
    bunyanLog.error(msg)
    if (info == TRUE) {
      cat(msg)
    }
    file.remove(filename) 
    return(FALSE)
  }
}


