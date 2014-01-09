# Roxygen Comments mantaSetLimits
#' Sets Manta durability level, connection timeouts and limits currently active
#'
#' Use mantaGetLimits to see the settings structure and default values.
#' Pass a structure in R or JSON to this function to change values.
#' This is where the Manta  default connection parameters are changed, 
#' including the number of copies of an object stored on the 
#' Manta service "durability_level" which can be from 2 to 6,
#' the number of directory entries retrieved in one http call "max_limit"
#' which is set to the maximum of 1000 by default. The other settings
#' recieve_timeout, sent_timeout and connect_timeout are for
#' http transfer sessions and are set with values in seconds. 
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
  durability_l <- 0
  if (is.na(charmatch("durability_level" ,headings)) == FALSE) {
    durability_l <- as.numeric(limits[charmatch("durability_level" ,headings)])
  }
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

  if ((durability_l >= 2) && (durability_l <= 6)) current_values$durability_level <- durability_l 
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

