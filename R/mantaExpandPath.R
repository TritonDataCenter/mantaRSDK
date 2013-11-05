# Roxygen Comments mantaExpandPath
#' Checks, expands ~~ and adds blank encoding to subdirectories
#'
#' Spaces in the directory path are substituted with %20
#' for HTTP i/o. Supports ~~ expansion to $MANTA_USER setting
#' Returns "" if directory specified incorrectory or
#' if the directory does not exist.
#'
#' @param m_path string, required.
#'
#' @param verbose logical, optional.
#'
#' @keywords Manta, manta
#'
mantaExpandPath <-
function(m_path, verbose = FALSE) {
    # handle ~~ expansion
    path <- sub("~~", manta_globals$manta_user, m_path)

    # needs leading "/" - add if missing
    if (substring(path,1,1) != "/") {
     path <- paste("/", path, sep="")
    }

    # remove trailing "/" if appended
    if (substring(path, nchar(path)) == "/") {
     path <- substring(path, 1, nchar(path)-1)
    }

    # must begin with /$MANTA_USER/stor or /$MANTA_USER/public
    lead <- paste("/",manta_globals$manta_user, sep="")
    lead_stor <- paste(lead, "/stor", sep="")
    lead_public <- paste(lead, "/public", sep="")
    lead_jobs <- paste(lead, "/jobs", sep="")
    lead_reports <- paste(lead, "/reports", sep="")
    if ( (is.na(charmatch(lead_stor,path))) && 
         (is.na(charmatch(lead_public,path))) &&
         (is.na(charmatch(lead_jobs,path))) &&
         (is.na(charmatch(lead_reports,path)))
       ) {
      if (verbose == TRUE) {
        cat("mantaRSDK:mantaExpandPath Error - Invalid subdirectory specified - must begin with:\n",
           lead_stor, " -or- ", 
           lead_public, " -or- ",
           lead_jobs, " -or- ",
           lead_reports, "\n")
      }
      return("")
    }

    # encode any spaces if foreign characters the string...
    path_enc <- curlEscape(path)   # this adds in unnecessary slash encoding, undo that.
    path_enc <- gsub("%2F","/",path_enc) 
    return(path_enc)
}
