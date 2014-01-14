# Roxygen Comments mantaExpandPath
#' Checks, expands \code{~~} to value of \code{$MANTA_USER}, and applies \code{curlEscape}.
#'
#' Not exported. 
#' Returns \code{""} if subdirectory specified incorrectory or
#' if the directory cannot exist on the system as specified.
#'
#' @param m_path character, required.
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
         (is.na(charmatch(lead_reports,path))) ) {
      msg <- paste("mantaExpandPath Error - Invalid subdirectory specified: ", path, "\n - must begin with:\n",
           lead_stor, " -or- ", 
           lead_public, " -or- ",
           lead_jobs, " -or- ",
           lead_reports, sep="")
      bunyanLog.error(msg)
      if (verbose == TRUE ) {
        cat(paste(msg,"\n"))
      }
      return("")
    }

    # encode any spaces if foreign characters the string...
    path_enc <- curlEscape(path)   # this adds in unwanted slash, ?, =  encoding, undo that.
    path_enc <- gsub("%2F","/",path_enc) 
    path_enc <- gsub("%3F","?",path_enc) # for conditionals
    path_enc <- gsub("%3D","=",path_enc) # for conditionals
    return(path_enc)
}
