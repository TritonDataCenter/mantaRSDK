# Roxygen Comments mantaSave.ws
#' Saves R workspace to Manta R workspace directory
#' with an audit trail of backups. 
#'
#' This function is a 
#' wrapper for mantaSave.image that handles the versioned default
#' Manta directory, and Snaplinks the last current.Rdata to a 
#' copy with [timestamp].Rdata before it is overwritten.
#'
#' @return logical.
#'
#' @keywords Manta, manta
#'
#' @export
mantaSave.ws <-
function() {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  

  ws_dir <- paste("/", manta_globals$manta_user, "/stor/", manta_globals$r_version, "/", manta_globals$hostname,  sep="")

  if (!mantaExists(ws_dir, d = TRUE)) {
   if ( mantaMkdir(ws_dir, p = TRUE, info = FALSE) == FALSE) {
      msg <- paste("mantaSave.ws: Unable to make user R workspace directory on Manta ", ws_dir, "\n", sep="")
      bunyanLog.error(msg)
      cat(msg)
      return(FALSE)
    }
  }

  mantapath <- paste(ws_dir, "/current.Rdata", sep = "")

  listing <- mantaLs(ws_dir, grepfor = "current[.]Rdata", ignore.case = FALSE, items = 'o', l = 'R')
  if (length(listing[[1]]) > 1) {
    if (! is.null(listing[[1]]$mtime) ) {
      # there is a current.Rdata object present, rename it to its timestamp.Rdata
      newname <- paste(ws_dir, "/", as.character(listing[[1]]$mtime) , " GMT.Rdata", sep="")
      newname <- gsub(" ","_",newname)
      mantaSnapln(mantapath, newname)
    }
  }

  return(mantaSave.image(mantapath))

}
