# Roxygen Comments mantaSetwd.ws
#' Sets the current Manta working directory to the R workspace e.g.  ~~/stor/R-3.0.1/ 
#'
#' @return logical.
#'
#' @keywords Manta, manta
#'
#' @export
mantaSetwd.ws <-
function() {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
  

  ws_dir <- paste("/", manta_globals$manta_user, "/stor/", manta_globals$r_version, "/", manta_globals$hostname, sep="")
  
  if (!mantaExists(ws_dir)) {
      msg <- paste("mantaLoad.ws: R workspace directory does not yet exist on Manta: ", ws_dir, " Use mantaSave.ws().\n", sep="")
      bunyanLog.error(msg)
      cat(msg)
      return(FALSE)
  }

 return(mantaSetwd(ws_dir))

}
