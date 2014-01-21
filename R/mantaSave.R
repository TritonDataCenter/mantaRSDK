# Roxygen Comments mantaSave
#' Uploads R data to Manta Storage Service using R function \code{save}.
#'
#' Uploads to R data files \code{.rda .Rdata .RData} files. If no
#' file extension is provided, \code{.rda} is appended. 
#' \code{mantaSave} is a wrapper for \code{save} and  \code{mantaXfer}, 
#' which implements the RCURL upload.
#'
#' @param ... See \code{save} R objects to be saved
#'
#' @param list required. See \code{save}  List of R objects to be saved.
#'
#' @param mantapath required. Path/filename to where uploaded data will go on 
#' Manta or Manta object/file name in current working Manta directory. If no 
#' extension is provided on the filename, or a non R data style extension 
#' \code{.rda} is appended to the end of the filename.  
#' 
#' @param md5 logical. Test md5 hash of R data tempfile with OpenSSL 
#' before/after PUT transfer. Default is \code{TRUE}. Setting \code{FALSE} will
#' speed up transfers a bit by skipping this step. 
#'
#' @param headers optional. Headers for HTTPS transfer, in \code{RCurl} style. 
#' See \code{\link{mantaPut}}. 
#' User metadata headers may be provided, E.g.:\cr
#  \code{headers = c('m-Title' = "Model Test", 'm-Iteration' = "42")}
#' Avoid supplying the \code{content-type} header, which is set to the R data type 
#' \code{"application/x-r-data"}, and the \code{durability-level} header 
#' which is handled via the \code{durability} parameter. 
#'
#' @param durability optional. Number of copies to store on Manta (2-6). If not
#' provided, uses saved value from \code{mantaSetLimits}, system default is 2.
#'
#' @param ascii optional. See \code{save}.
#'
#' @param version optional. See \code{save}.
#'
#' @param envir optional. See \code{save}. Environment of R object being passed.
#'
#' @param compress optional. See \code{save}.
#'
#' @param compression_level optional. See \code{save}.
#'
#' @param eval.promises optional. See \code{save}.
#'
#' @param precheck optional. See \code{save}.
#'
#' @param info logical required. Set to FALSE to silence output messages while downloading.
#'
#' @param verbose logical, optional. Passed to \code{RCurl} \code{GetURL},
#' Set to \code{TRUE} to see background REST communication on \code{stderr}.
#' Note this is not visible on Windows.
#' 
#' @return \code{TRUE} or \code{FALSE} depending on success of transfer.
#'
#' @keywords Manta
#'
#' @family mantaGet
#'
#' @seealso \code{\link{mantaLoad}}
#'
#' @examples
#' \dontrun{
#' data <- runif(100)
#' mantaSave("data", mantapath = "~~/stor/data")
#' rm(data)
#' mantaExists("~~/stor/data.rda")
#' mantaLoad("~~/stor/data.rda")
#' ls()
#' rm(data)
#' }
#'
#' @export
mantaSave <-
function(..., list = character(), mantapath = stop("'mantapath' destination file or full path must be specified"), md5 = FALSE,
         headers, durability = 2, ascii = FALSE, version = NULL, envir = parent.frame(), compress = !ascii, compression_level,
         eval.promises = TRUE, precheck = TRUE, info = TRUE, verbose = FALSE) {


  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }



  if ( substr(mantapath, nchar(mantapath), nchar(mantapath)) == "/" )  {
    stop("mantaSave. Cannot resolve target file name to store R memory object on Manta - mantapath ends in /")
  }

  temp_f <- tempfile()

  save(..., list = list, file = temp_f, ascii = ascii, version = version, envir = envir, compress = compress, 
       compression_level = compression_level, eval.promises = eval.promises, precheck = precheck) 
    

  # File extensions conforming to RData type 
  pathsplit <- strsplit(mantapath,"/")
  to_filename <- pathsplit[[1]][length(pathsplit[[1]])]
  extensions <- c( "rda", "RData", "Rdata", "rdata") 
  ## does filename have extension, if not append one
  namesplit <- strsplit(to_filename,"[.]")
  if (length(namesplit[[1]]) > 1) { #extension present
    # case where supplied is ".Rdata" gives a count of 2, so this is ok - 
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


  path_enc <- mantaExpandPath(mantapath) # is mantapath a valid mantapath?
  if (path_enc == "") {  # nope,  assume path from working directory
    mantapath <- paste(mantaGetwd(), "/", mantapath, sep = "")
    path_enc <- mantaExpandPath(mantapath) 
  }

  if (path_enc == "") {
    msg <- paste("mantaSave - Argument error - mantapath: [", mantapath, "]\n", sep = "")
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

