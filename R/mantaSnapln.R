# Roxygen Comments mantaSnapln
#' Makes a Snaplink - combination snapshot and symbolic link. 
#'
#'
#' Example: mantaSnapln(mantaLs(items = 'o'), "~~/public/share") will
#' make SnapLinks for all objects in the current directory located
#' in the "~~/public/share" directory. 
#' If the original object is overwritten/deleted, the SnapLink
#' still contains the object contents at time of creation.
#'
#' @param from character, required. Object in current subdirectory
#' or full Manta path to stored object. Vectorized.
#'
#' @param to character, required. Snaplink name in current subdirectory
#' or full Manta object path to the new SnapLink. If from is used as vectorized
#' list of mantapaths, to must specify a single Manta directory 
#' ending with / character. 
#'
#' @param info logical. When FALSE suppresses messages on the console.
#'
#' @keywords Manta, manta
#'
#' @export
mantaSnapln <-
function(from, to, info = TRUE) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

 if (missing(from) || missing(to)) {
   cat("mantaSnapln Error - Missing argument - from or to")
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

 if ((from_path_enc != "") && (to_path_enc != "")) {
   headers <- c('content-type' = "application/json; type=link",
                'Location' = from_path_enc)
   return(mantaAttempt(action=to_path_enc, method = "PUT", headers = headers, test = TRUE, returncode = "204"))
 }

 return(FALSE)
}

