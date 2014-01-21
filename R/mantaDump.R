# Roxygen Comments mantaDump
#' Uses \code{dump} to upload text parsable R data to Manta Storage Service.
#'
#' \code{mantaDump} is a wrapper for \code{dump} and \code{mantaXfer}, 
#' which implements the RCURL transfer
#'
#' @param list required.  See \code{dump}. List of R objects to dump. Name of R object in 
#' quotes works as well.
#'
#' @param mantapath optional. Default is \code{dumpdata.R}. 
#' Specify full Manta path to where uploaded R source will be saved 
#' or Manta object name in current working Manta directory. If no 
#' extension is provided on the filename, or a non R data style extension, 
#' \code{.R} is appended to the end of the filename.  
#' 
#' @param md5 logical. Test \code{md5} hash of R dump tempfile before/after upload.
#'
#' @param headers  Headers for HTTP transfer, in \code{RCurl} style. 
#' See \code{\link{mantaPut}}. \cr
#' User metadata headers may be provided, E.g.:\cr
#  \code{headers = c('m-Title' = "Model Fitting Test", 'm-Iteration' = "42")}\cr
#' Avoid supplying the \code{content-type} header, which is set to the R source code 
#' \code{text/R-code} and the \code{durability-level} header which is handled 
#' via the \code{durability} parameter. 
#'
#' @param durability optional. Number of copies to store on Manta (2-6). If not
#' provided, uses saved value from \code{\link{mantaSetLimits}}, system default is 2.
#'
#' @param envir optional. See \code{dump}. Environment of R object being passed.
#'
#' @param control optional.  See \code{dump}.
#'
#' @param evaluate optional.  See \code{dump}.
#'
#' @param info logical required. Set to \code{FALSE} to silence console output messages.
#'
#' @param verbose logical, optional. Passed to \code{RCurl} \code{GetURL},
#' Set to \code{TRUE} to see background HTTPS REST communication on \code{stderr}
#' Note this is not visible on Windows.
#' 
#' @return \code{TRUE} or \code{FALSE} depending on success of upload.
#'
#' @keywords Manta
#'
#' @family mantaPut
#'
#' @seealso \code{\link{mantaSource}}
#'
#' @examples
#' \dontrun{
#' data <- runif(100)
#' mantaDump("data")
#' mantaCat("dumpdata.R")
#' mantaRm("dumpdata.R")
#' }
#'
#' @export
mantaDump <-
function(list, mantapath = "dumpdata.R", md5 = FALSE,
         headers, durability = 2, envir = parent.frame(), control = "all", 
         evaluate = TRUE,  info = TRUE, verbose = FALSE) {


  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }


  if ( substr(mantapath, nchar(mantapath), nchar(mantapath)) == "/" )  {
    stop("mantaDump. Cannot resolve target file name to store R memory object on Manta - mantapath ends in /")
  }

  if (missing(list)) stop("mantaDump. No objects specified in list.")

  ex <- sapply(list, exists, envir = envir)
  if (!any(ex))  stop("mantaDump. No objects specified in list.")
  temp_f <- tempfile()
  temp_con <- file(temp_f, "w")
  dump(list = list, file = temp_con,  append = FALSE, control = control, 
    envir = envir, evaluate = evaluate) 
  flush(temp_con)
  close(temp_con)

  # File extensions conforming to .R type 
  pathsplit <- strsplit(mantapath,"/")
  to_filename <- pathsplit[[1]][length(pathsplit[[1]])]
  extensions <- c( "r", "R") 
  ## does filename have extension, if not append one
  namesplit <- strsplit(to_filename,"[.]")
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

  content_header <- c('content-type' = "text/R-code")
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

  msg <- paste("Uploading R dump text to Manta: ", mantapath, "\n",  sep="")
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

