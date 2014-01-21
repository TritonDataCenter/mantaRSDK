# Roxygen Comments mantaGetLimits
#' Returns Manta durability level, connection timeouts and limits currently active.
#'
#' Reports the mantaRSDK settings structure and default/current values.
#' Includes the number of copies of an object stored on the 
#' Manta service \code{durability_level} which can be from 2 to 6,
#' the number of directory entries retrieved in one HTTPS call \code{max_limit}
#' set to the maximum of 1000 by default. The other settings
#' \code{recieve_timeout, sent_timeout} and \code{connect_timeout} are for
#' HTTPS transfer sessions and are set with values in seconds. 
#'
#' @param all logical, optional, Get all limits values
#'
#' @param durability_level logical, optional. Get durability level.
#'
#' @param connect_timeout logical, optional. Get connect timeout.
#'
#' @param send_timeout, logical, optional. Get send timeout.
#'
#' @param receive_timeout, logical, optional. Get recieve timeout.
#'
#' @param max_limit logical, optional. Get the maximum number of directory 
#' entries transferred in one HTTPS call (upper limit 1000 is the default)
#'
#' @param json logical, optional. Set \code{TRUE} to return values in JSON
#'
#' @return JSON or R values as specified.
#'
#' @family mantaAccount
#'
#' @seealso \code{\link{mantaSetLimits}}
#'
#' @examples
#' \dontrun{
#' ## Save all current settings with:
#' limits <- mantaGetLimits(all = TRUE)
#' limits
#' ## Change one or all settings, set with:
#' mantaSetLimits(limits)
#' }
#'
#' @keywords Manta
#'
#' @export
mantaGetLimits <-
function(all = TRUE,
         durability_level = FALSE,
         connect_timeout = FALSE,
         send_timeout = FALSE,
         receive_timeout = FALSE,
         max_limit = FALSE,
         json = FALSE) {

  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  durability_l <- list()
  connect_t <- list()
  send_t <- list()
  receive_t <- list()
  max_n <- list()

  if (all == TRUE) {
    durability_level <- TRUE
    connect_timeout <- TRUE
    send_timeout <- TRUE
    receive_timeout <- TRUE
    max_limit <- TRUE
  }

  all_values <- manta_globals$manta_defaults

  if (durability_level == TRUE) {
    durability_l <- list(durability_level = all_values$durability_level)
  }

  if (connect_timeout == TRUE) {
    connect_t <- list(connect_timeout = all_values$connect_timeout)
  }

  if (send_timeout == TRUE) {
    send_t <- list(send_timeout = all_values$send_timeout)
  }

  if (receive_timeout == TRUE) {
    receive_t <- list(receive_timeout = all_values$receive_timeout)
  }

  if (max_limit == TRUE) {
    max_n <- list(max_limit = all_values$max_limit)
  }

  limitslist <- c(durability_l, connect_t, send_t, receive_t, max_n)
  if (json == TRUE) {
    return(toJSON(limitslist))
  } else {
    return(limitslist)
  }
}

