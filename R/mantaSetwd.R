# Roxygen Comments mantaSetwd
#' Sets current working directory on Manta.
#'            
#' This sets the current working directory in Manta.
#' Supports ~~ expansion to \code{$MANTA_USER} setting,
#' i.e. for my account \code{~~/stor} expands to 
#' \code{/cwvhogue/stor}.
#' There are 4 top level Manta subdirectories:\cr
#' \code{~~/stor} Your private storage.\cr
#' \code{~~/public} Your public storage.\cr
#' \code{~~/jobs} Your job archive.\cr
#' \code{~~/reports} Your account report information.\cr
#' Specify the full path (absolute) or start from current 
#' working directory (relative). All mantaRSDK functions assume
#' unprefixed filenames are in the current working directory
#' on Manta. To move UP one directory at a time use 
#' \code{".."} but note that \code{"../.."} forms are NOT 
#' supported as there are no parent directory 
#' \code{".."} object links on Manta. 
#' Returns FALSE if directory specified incorrectory or
#' if the directory does not exist.  
#' The current working directory is stored internally in
#' \code{mantaRSDK} on your local system and is not saved.
#' between sessions. It initializes to the root directory
#' of private Manta storage: \code{~~/stor}.     
#'
#' @param mantapath character, required. Absolute or
#' relative subdirectory name to set to. 
#' 
#' @keywords Manta, manta
#'
#' @family Directory
#'
#' @examples
#' \dontrun{
#' ## Show current Manta working directory
#' mantaGetwd()   
#' ## Save current subdirectory
#' mantaGetwd() -> tempdir
#' ## Absolute path with ~~ expansion
#' mantaSetwd("~~/public")
#' mantaGetwd()
#' ## Dotted forms for 4 top level subdirectories:
#' mantaSetwd.public()
#' mantaGetwd()
#' mantaLs.l()
#' mantaSetwd.stor()
#' mantaGetwd()
#' mantaLs.l()
#' mantaSetwd.jobs()
#' mantaGetwd()
#' mantaLs.l()
#' mantaSetwd.reports()
#' mantaGetwd()
#' mantaLs.l()
#' ## Restore saved subdirectory
#' mantaSetwd(tempdir)
#' }
#'
#'
#' @export
mantaSetwd <-
function(mantapath) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  if (missing(mantapath)) {
    cat("mantaSetwd Error - no subdirectory specified")
    return(FALSE)
  }
    
  if (mantapath == "..")  { # can we go up ?
    # go up one level, but not supporting ../../../ 
    # a bit janky but useful
    path <- mantaGetwd()
    splitpath <- strsplit(path, split="/")
    path <- paste(splitpath[[1]][-length(splitpath[[1]])], collapse="/")
    path_enc <- mantaExpandPath(path)
    # So mantaExpandPath will return "" if user 
    # tries to go up level from /stor or /public
    if (path_enc == "") return(FALSE)
  } else {
    path_enc <- mantaPath(mantapath)
  }

  # is it really there ?
  if (mantaExists(curlUnescape(path_enc), d = TRUE) == TRUE) {
    assign("manta_cwd", path_enc, envir = manta_globals)
    return(TRUE)
  } else {
    msg <- paste("mantaSetwd Cannot change to a missing subdirectory ", mantapath, sep="")
    bunyanLog.info(msg)
    cat(msg,"\n")
    return(FALSE)
  }
}
