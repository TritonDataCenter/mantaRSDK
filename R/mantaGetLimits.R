# Roxygen Comments mantaGetLimits
# TODO: organize roxygen with examples
#' Returns Manta connection timeouts and limits  currently active
#'
#' Save all current settings with:
#'
#' limits <- mantaGetLimits(all = TRUE)
#'
#' Change one or all settings, set with:
#'
#' mantaSetLimits(limits)
#'
#' @param all logical, optional, TRUE by default all limit settings
#'
#' @param connect_timeout logical, optional. Set TRUE to retrive
#'
#' @param send_timeout, logical, optional. Set TRUE to retrieve
#'
#' @param receive_timeout, logical, optional. Set TRUE to retrive
#'
#' @param max_limit logical, optional. Set TRUE to get the max dir limit
#'
#' @param json logical, optional. Set TRUE to get JSON output
#'
#' @return JSON or R values as specified.
#'
#' @keywords Manta, manta
#'
#' @export
mantaGetLimits <-
function(all = TRUE,
         connect_timeout = FALSE,
         send_timeout = FALSE,
         receive_timeout = FALSE,
         max_limit = FALSE,
         json = FALSE) {

  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  connect_t=list()
  send_t=list()
  receive_t=list()
  max_n=list()

  if (all == TRUE) {
    connect_timeout <- TRUE
    send_timeout <- TRUE
    receive_timeout <- TRUE
    max_limit <- TRUE
  }

  all_values <- manta_globals$manta_defaults

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

  limitslist <- c(connect_t, send_t, receive_t, max_n)
  if (json == TRUE) {
    return(toJSON(limitslist))
  } else {
    return(limitslist)
  }
}

