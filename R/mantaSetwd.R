# Roxygen Comments mantaSetwd
#' Sets Manta working directory 
#'
#' This sets the current working directory in Manta
#' Spaces in the directory path are substituted with %20
#' for HTTP i/o. Supports ~~ expansion to $MANTA_USER setting
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
    if (missing(mantapath)) {
     cat("mantaRSDK:mantaSetwd Error - no subdirectory specified")
     return(FALSE)
    }

    # handle ~~ expansion
    path <- sub("~~",manta_globals$manta_user, mantapath)

    # needs leading "/" - add if missing
    if (substring(path,1,1) != "/") {
     path <- paste("/",path,sep="")
    }

    # remove trailing "/" if appended
    if (substring(path,nchar(path)) == "/") {
     path <- substring(path,1,nchar(path)-1)
    }

    # must begin with /$MANTA_USER/stor or /$MANTA_USER/public 
    lead <- paste("/",manta_globals$manta_user,sep="")
    lead_stor <- paste(lead,"/stor",sep="")
    lead_public <- paste(lead,"/public",sep="")
    if ( (is.na(charmatch(lead_stor,path))) && (is.na(charmatch(lead_public,path)))) {
      cat("mantaRSDK:mantaSetwd Error - Invalid subdirectory specified - must begin with:\n",
           lead_stor, " -or- ", lead_public, "\n")
      return(FALSE)
    } 

    # encode any spaces in the string with %20
    path_enc <- sub(" ","%20",path)

    # is it really there ?
    if (mantaAttempt(action=path_enc, method="GET", test = TRUE) == TRUE) {
      assign("manta_cwd", path_enc, envir=manta_globals)
      return(TRUE)
    } else {
      cat("mantaRSDK:mantaSetwd Cannot change to missing subdirectory ",path," \n")
      return(FALSE)
    }
}
