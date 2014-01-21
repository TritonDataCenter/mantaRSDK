# Roxygen Comments mantaWhoami
#' Reports the active Manta account information.
#'
#' The active Manta account is initially obtained from
#' environment variables \code{$MANTA_USER}, \code{$MANTA_KEY}, 
#' and \code{$MANTA_URL}. 
#' Retrieve account settings, data center as JSON with this function.
#' Change/restore account settings with \code{mantaAccount}.
#'
#' @param all logical, optional, \code{TRUE} returns all account settings.
#'
#' @param user logical, optional. \code{TRUE} by default to report Manta user.
#'
#' @param dc_url logical, optional. Set \code{TRUE} to get Manta data center.
#'
#' @param key_id logical, optional. Set \code{TRUE} to get the current key id.
#'
#' @param ssl_key logical, optional. Set \code{TRUE} to get the private key path.
#'
#' @param json logical, optional. Set \code{TRUE} to get JSON output
#'
#' @return JSON or R values as specified.
#'
#' @keywords Manta
#'
#' @family mantaAccount
#'
#' @examples
#' \dontrun{
#' mantaWhoami()
#' ## To see/save current account settings:
#' account <- mantaWhoami(all = TRUE)
#'
#' ## then use:  
#'
#' mantaAccount(account) ## to set the modified account
#'
#' ## Account information may contain 1-4 key-value pairs.
#'
#' ## To see/save current account settings as JSON:
#'
#' account <- mantaWhoami(all = TRUE, json = TRUE)
#'
#' ## then use:  
#'
#' mantaAccount(json = account) to set that account
#'
#' }
#' 
#'
#' @export
mantaWhoami <-
function(all = FALSE,
         user = TRUE, 
         dc_url = FALSE, 
         key_id = FALSE, 
         ssl_key = FALSE, 
         json = FALSE) {
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
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
