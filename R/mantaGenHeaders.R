# Roxygen Comments mantaGenHeaders
#' Create all Headers for Manta REST requests
#'
#' Creates User Agent, Authorization, Date headers.
#' Calls \code{\link{mantaGenSignature}} to generate timestamp
#' and authorization signature.
#'
#' @return manta_headers HTTP headers as specified for RCurl
#'
#' @keywords Manta, manta
#'
mantaGenHeaders <-
function() {
  # TODO: 
  #   Supply mantaRSDK version number information

  alg<-"rsa-sha256"
  r_version <- as.character(getRversion())
  user_agent <- paste("R-",r_version, "/mantaRSDK", sep="")

  signed <- mantaGenSignature()
  the_time_now <- signed$time_now
  sig_encrypted <- signed$signature

  auth_header <- paste("Signature keyId=", 
                       '\"', 
                       manta_globals$manta_key_path, 
                       '\",',
                       'algorithm=\"', 
                       alg, 
                       '\"', 
                       ',signature=\"',
                       sig_encrypted,
                       '\"', 
                       sep="")

  manta_headers <- c('User-Agent' = user_agent , 
                              date = the_time_now, 
                     Authorization = auth_header)
  return(manta_headers)
}