# Roxygen Comments mantaSnapln
#' Makes a Snaplink - combination ZFS snapshot and Symbolic link. 
#'
#' As a persistent object store, there are no copy or move commands on Manta. 
#' Instead the \code{mantaSnapln} command 
#' is used to add an object's name into another subdirectory without
#' physically moving data on the service. Internally the system
#' takes a ZFS snapshot of the data and the new object entry is 
#' the snapshot. If the original data is overwritten, the SnapLink
#' still points to the original snapshot. The R workspace audit trail 
#' used by
#' \code{\link{mantaSave.ws}} and \code{\link{mantaLoad.ws}} is
#' implemented using \code{mantaSnapln}.
#' 
#' @param from character, required. Object in current subdirectory
#' or full Manta path to stored object. Vectorized.
#'
#' @param to character, required. Snaplink name in current subdirectory,
#' existing Manta subdirectory
#' or full Manta object path to the new SnapLink. If \code{from} is a 
#' vector of Manta paths, 
#' then \code{to} must specify a single valid Manta 
#' subdirectory. 
#'
#' @param info logical. When FALSE suppresses messages on the console.
#'
#' @family Directory
#'
#' @seealso \code{\link{mantaSave.ws}} \code{\link{mantaLoad.ws}}
#'
#' @keywords Manta
#'
#' @examples
#' \dontrun{
#' ## Save a static hello world HTML page
#' htmlpage <- paste("<!DOCTYPE html>\n<html>\n<body>\n\n",
#'                   "<h1>Hello from Joyent Manta.</h1>\n\n",
#'                   "<p>Hello world! from ",
#'                   mantaWhoami(), 
#'                   ".</p>\n\n",
#'                   "</body>\n</html>", sep="")
#' file <- file("test_index.html", "wb")
#' write(htmlpage,file)
#' close(file)
#' rm(file)
#' rm(htmlpage)
#'
#' ## Upload the HTML file to Manta in your private area
#' mantaSetwd.stor()
#' mantaPut("test_index.html")
#' mantaCat("test_index.html")
#'
#' ## Make it public
#' mantaMkdir("~~/public/test")
#' mantaSnapln("test_index.html", "~~/public/test")
#' mantaSnapln("test_index.html", "~~/public/test/index.html")
#'
#' ## copy and paste URL into browser.
#' mantaLs.url("~~/public/test", grepfor = "[.]html")
#'
#' ## Delete the original in private area
#' mantaRm("~~/stor/test_index.html")
#' mantaExists("~~/stor/test_index.html")
#'
#' ## Snaplink copies is still there in ~~/public
#' mantaExists("~~/public/test/test_index.html")
#' mantaExists("~~/public/test/index.html")
#' mantaCat("~~/public/test/index.html")
#'
#' ## Cleanup this demo 
#' # mantaRm("~~/public/test/index.html")
#' # mantaRm("~~/public/test/test_index.html")
#' # mantaRmdir("~~/public/test")
#' # file.remove("test_index.html")
#' }
#'
#' @export
mantaSnapln <-
function(from, to, info = TRUE) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(from) || missing(to)) {
   msg <- ("mantaSnapln Error - Missing argument - from and to must be specified\n")
   bunyanLog.error(msg)
   stop(msg)
   return(FALSE)
 }

 if ( (length(from) > 1) && (length(to) == 1) ) {
   # Vectorize 
   # Check to has last character "/" 
   if (substr(to, nchar(to), nchar(to)) != "/") {
      to <- paste(to, "/", sep="")
   } 
   to_path_enc <- mantaPath(to)
   if (mantaExists(curlUnescape(to_path_enc), d = TRUE) == FALSE) {
    msg <- paste("mantaSnapln Destination Directory Error: ", to , "\n does not exist. Use mantaMkdir() first.", sep="")
    bunyanLog.error(msg)
    stop(msg)
   } 
   # Expand from
   frompaths <- unlist(lapply(from, mantaPath)) # this expands and escapes !
   fromfiles <- unlist(lapply(frompaths, strsplit, split="/"), recursive = FALSE)
   filenames <-  vector("list", length(from))
   for (i in 1:length(fromfiles)) {
     filenames[[i]] <- fromfiles[[i]][length(fromfiles[[i]])]
   }
   filenames <- unlist(filenames)
   topaths <- paste(curlEscape(to), filenames, sep="") # these are escaped.
   retval <- vector("logical", length(frompaths))
   for (i in 1:length(frompaths)) {
     retval[i] <-  mantaSnapln(curlUnescape(frompaths[i]), curlUnescape(topaths[i]))
     Sys.sleep(0.5)  # Windows gets way ahead of itself here... 
     msg <- paste(curlUnescape(frompaths[i]), " -SnapLinked to- ", curlUnescape(topaths[i]), "\n", sep="")
     bunyanLog.info(msg)
     if (info == TRUE) {
       cat(msg)
     }
   }
   return(retval)
 }


 from_path_enc <- mantaPath(from)

 to_path_enc <- mantaPath(to)

 # is to a directory?
 if (mantaExists(curlUnescape(to_path_enc), d = TRUE) == TRUE) {
    # Check to has last character "/" 
    if (substr(to, nchar(to), nchar(to)) != "/") {
      to <- paste(to, "/", sep = "")
    }  
    # grab filename at end of from.
    fromsplit <- unlist(strsplit(from_path_enc, split = "/"))
    fromfilename <- fromsplit[length(fromsplit)]
    to_path <- paste(to, fromfilename, sep="")
    to_path_enc <- mantaPath(to_path)
  } # else it is assumed to specifies a file ...

 if ((from_path_enc != "") && (to_path_enc != "")) {
   headers <- c('content-type' = "application/json; type=link",
                'Location' = from_path_enc)
   return(mantaAttempt(action=to_path_enc, method = "PUT", headers = headers, test = TRUE, returncode = "204"))
 }

 return(FALSE)
}

