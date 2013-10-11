# Roxygen Comments mantaWhoami
# TODO: organize roxygen with examples
#' Returns Manta username currently active
#'
#' Report the active Manta account. This is initially obtained from
#' environment variables MANTA_USER, MANTA_KEY, MANTA_URL. 
#' Save account settings, data center as JSON with this function.
#' Change/restore account settings with mantaAccount and JSON values.
#' 
#' Save all current settings with:
#' 
#' account <- mantaWhoami(all = TRUE)
#'
#' Restore that account with:
#'
#' mantaAccount(account)
#'
#' Get current Manta Datacenter only, as JSON:
#' mantaWhoami(dc_url=TRUE, user=FALSE, json=TRUE)
#'
#'
#' @param all logical, optional, TRUE returns all account settings
#'
#' @param user logical, optional. TRUE by default to report Manta user
#'
#' @param dc_url logical, optional. Set TRUE to get Manta data center
#'
#' @param key_id logical, optional. Set TRUE to get the current key id
#'
#' @param ssl_key logical, optional. Set TRUE to get the private key path
#'
#' @param json logical, optional. Set TRUE to get JSON output
#'
#' @return JSON or R values as specified. 
#'
#' @keywords Manta, manta
#'
#' @export
mantaWhoami <-
function(all = FALSE,
         user = TRUE, 
         dc_url = FALSE, 
         key_id = FALSE, 
         ssl_key = FALSE, 
         json = FALSE) {
  if (manta_globals$manta_ok == FALSE) {
    stop("Manta not Initialized.\nSee: help(mantaInit)\n")
  }
  keylist=list()
  urllist=list()
  userlist=list()
  sslkeylist=list()
  if (all == TRUE) {
    user <- TRUE
    dc_url <- TRUE
    key_id <- TRUE
    ssl_key <- TRUE
  }
  if (ssl_key == TRUE) {
    pk <- get('ssl_key_path', manta_globals)
    sslkeylist <- list(SSL_KEY_PATH =  pk)
  }  
  if (key_id == TRUE) {
    key <- get('manta_key_id', manta_globals)
    keylist <- list(MANTA_KEY_ID =  key)
  }  
  if (dc_url == TRUE) {
    url <- get('manta_url', manta_globals)
    urllist <- list(MANTA_URL = url)
  }
  if (user == TRUE) {
    userid <- get('manta_user', manta_globals)
    userlist <- list(MANTA_USER = userid)
  }
  whoamilist <- c(userlist,urllist,keylist,sslkeylist)
  if (json == TRUE) {
    return(toJSON(whoamilist))
  } else {
    return(whoamilist)
  }
}
