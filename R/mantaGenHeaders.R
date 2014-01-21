# Roxygen Comments mantaGenHeaders
#' Create Headers for Manta HTTPS REST requests
#'
#' Not exported.
#' Creates \code{User-Agent, Authorization, date} headers.
#' Calls \code{\link{mantaGenSignature}} to generate \code{date}
#' and \code{Authorization} signature using \code{openSSL}.
#'
#' @return manta_headers HTTP headers as specified for \code{RCurl}
#'
#' @keywords Manta
#'
mantaGenHeaders <-
function() {

  alg<-"rsa-sha256"
   
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

  manta_headers <- c('User-Agent' = manta_globals$user_agent , 
                              date = the_time_now, 
                     Authorization = auth_header)
  return(manta_headers)
}
