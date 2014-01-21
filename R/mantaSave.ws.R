# Roxygen Comments mantaSave.ws
#' Save current R workspace and uploads to a  Manta R workspace directory with audit trail.
#'
#' Uploads Manta \code{"current.Rdata"} object to an audit-trail workspace directory
#' using \code{mantaSave.image}.
#'
#' Together with \code{mantaLoad.ws} this function works with an audit trail of workspaces
#' maintined in a Manta subdirectory created by \code{mantaSave.ws} 
#' made by R version and  mantaRSDK client hostname, that looks like these:\cr
#' \code{~~/stor/R-3.0.1/cwvh-macbookpro/} \cr
#' \code{~~/stor/R-3.0.2/CHOGUE-HPDV7/} \cr
#' These workspace archive subdirectories are made by \code{\link{mantaSave.ws}}
#' when first run.\cr
#' The last saved R workspace from these two systems in each directory     
#' is named \code{"current.Rdata"}. Previously saved R workspaces are renamed
#' to their original GMT creation date/time stamp on Manta 
#' and archived with SnapLinks before
#' writing a new workspace.\cr
#' Archived workspaces are named like this:\cr 
#' \code{"2014-01-07_14:53:05_GMT.Rdata"}
#' To retrieve an older workspace or a workspace saved from a different mantaRSDK
#' client and version, use this form:\cr
#' \code{mantaLoad("~~/stor/R-3.0.0/hostname/2014-01-07_14:53:05_GMT.Rdata"}
#' If you wish to save your workspace to a different 
#' location use \code{\link{mantaSave.image}}
#' Adds appropriate \code{content-type} HTTP header, which is set
#' to \code{"application/x-r-data"}.
#'
#' @return \code{TRUE} or \code{FALSE} depending on success of download.
#'
#' @family mantaPut
#'
#' @keywords Manta
#'
#' @seealso \code{\link{mantaSave.image}} \code{\link{mantaLoad}} \code{\link{mantaSetwd.ws}}
#' \code{\link{mantaLoad.ws}}
#'
#' @examples
#' \dontrun{
#' somedata <- runif(100)
#' somechar <- "My current workspace"
#' ls()
#' mantaSave.ws()                                           
#' rm(somedata)
#' rm(somechar)
#' mantaLoad.ws()           
#' ls()
#' ## What is my workspace subdirectory?
#' mantaGetwd() -> tempdir
#' mantaSetwd.ws()
#' mantaGetwd()   ## this one
#' mantaLs.l()    ## Inspect workspace archives
#' mantaSetwd(tempdir)  
#' }
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
