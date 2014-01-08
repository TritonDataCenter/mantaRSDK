# Roxygen Comments mantaLoad.ws
#' Loads current R workspace from Manta R workspace directory
#'
#' @param envir optional. Enviroment into which workspace is loaded.
#'
#' @return logical.
#'
#' @keywords Manta, manta
#'
#' @export
mantaLoad.ws <-
function(envir = parent.frame()) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  

  ws_dir <- paste("/", manta_globals$manta_user, "/stor/", manta_globals$r_version, "/", manta_globals$hostname,  sep="")

  if (!mantaExists(ws_dir)) {
      msg <- paste("mantaLoad.ws: R workspace directory does not yet exist on Manta: ", ws_dir, " Use mantaSave.ws().\n", sep="")
      bunyanLog.error(msg)
      cat(msg)
      return(FALSE)
  }

  mantapath <- paste(ws_dir, "/current.Rdata", sep="")

  listing <- mantaLs(ws_dir, grepfor = "current[.]Rdata", ignore.case = FALSE, items = 'o', l = 'R')
  if (length(listing[[1]]) > 1) {
    if (! is.null(listing[[1]]$mtime) ) {
      return(mantaLoad(mantapath, envir = envir))
    }
  }

 msg <- paste("mantaLoad.ws: current R workspace object not found on Manta:", mantapath, "\n", sep="")
 bunyanLog.error(msg)
 cat(msg)
 return(FALSE)

}
