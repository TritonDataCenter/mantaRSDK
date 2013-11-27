# Roxygen Comments mantaSetwd
#' Sets Manta working directory 
#'
#' This sets the current working directory in Manta
#' Supports ~~ expansion to $MANTA_USER setting
#' Specify the full path or start from current working directory
#' Specify ".." to move up ONE directory at a time.
#' Returns FALSE if directory specified incorrectory or
#' if the directory does not exist.  
#'
#' @param mantapath string, required.
#' 
#' @keywords Manta, manta
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
  if (mantaAttempt(action=path_enc, method="GET", test = TRUE, silent = TRUE) == TRUE) {
    assign("manta_cwd", path_enc, envir = manta_globals)
    return(TRUE)
  } else {
    msg <- paste("mantaSetwd Cannot change to a missing subdirectory ", mantapath, sep="")
    bunyanLog.info(msg)
    cat(msg,"\n")
    return(FALSE)
  }
}
