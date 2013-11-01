# Roxygen Comments mantaSetLimits
# TODO: organize roxygen with examples
#' Sets Manta connection timeouts and limits currently active
#'
#' Save all current settings with:
#'
#' limits <- mantaGetLimits(all = TRUE)
#'
#' Change one or all settings, set with:
#'
#' mantaSetLimits(limits)
#'
#' @param limits list,  optional. R values
#'
#' @param json character, optional. JSON limits values
#'
#' @param verbose logical, optional. TRUE by default
#'
#' @return logical TRUE if values changed
#' FALSE if values unchanged
#'
#' @keywords Manta, manta
#'
#' @export
mantaSetLimits <-
function(limits, json, verbose=FALSE) {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  if (missing(limits) && missing(json)) {
      stop("mantaSetLimits: No limit information provided.\nSee: mantaGetLimits\n")
  }

  if (missing(limits)) {
    # do we have JSON input?
    if (isValidJSON(json, asText = TRUE)) {
       limits <- fromJSON(json)
    } else {
       stop(paste("Invalid JSON content:\n",json,"\n",sep=""))
    }
  }

  current_values <- manta_globals$manta_defaults

  if (verbose == TRUE) {
  cat("Changing Limits From:\n")
  cat(toJSON(current_values))
  cat("\n")
  }

  # Check account contents
  headings <- names(limits)
  if (length(headings) == 0)
    stop("Incorrect limit structure\nSee: mantaGetLimits\n")

  # Fetch and assign new values as provided
  connect_t <- 0
  if (is.na(charmatch("connect_timeout" ,headings)) == FALSE) {
    connect_t <- as.numeric(limits[charmatch("connect_timeout" ,headings)])
  }
  send_t <- 0
  if (is.na(charmatch("send_timeout" ,headings)) == FALSE) {
    send_t <- as.numeric(limits[charmatch("send_timeout" ,headings)])
  }
  receive_t <- 0
  if (is.na(charmatch("receieve_timeout" ,headings)) == FALSE) {
    receive_t <- as.numeric(limits[charmatch("receive_timeout" ,headings)])
  }
  max_n <- 0
  if (is.na(charmatch("max_limit" ,headings)) == FALSE) {
    max_n <- as.numeric(limits[charmatch("max_limit" ,headings)])
  }

  if (connect_t > 0) current_values$connect_timeout <- connect_t
  if (send_t > 0) current_values$send_timeout <- send_t
  if (receive_t > 0) current_values$receive_timeout <- receive_t
  if (max_n > 0) current_values$max_limit <- max_n
  
  assign("manta_defaults", current_values, envir=manta_globals)

  if (verbose == TRUE) {
  cat("To:\n")
  cat(toJSON(manta_globals$manta_defaults))
  cat("\n")
  }

}

