# Roxygen Comments mantaGetLimits
#' Returns Manta durability level, connection timeouts and limits currently active.
#'
#' Use mantaGetLimits to see the settings structure and default values.
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
#' @param all logical, optional, TRUE by default all limit settings
#'
#' @param durability_level logical, optional. Set TRUE to retrive
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

