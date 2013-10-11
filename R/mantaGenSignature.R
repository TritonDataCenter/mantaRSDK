
# Roxygen Comments mantaGenSignature
#' Get time, digest and encrypt for HTTPS authentication
#'
#' Gets system time and converts to web time format.
#' Implements HTTPS Signature according to Mark Cavage Draft
#' http://tools.ietf.org/html/draft-cavage-http-signatures-00
#'
#' @return signed time and rsa-sha256 signature
#'
#' @keywords Manta, manta
#'
mantaGenSignature <-
function() {
  # TODO: 
  #   Windows openssl / or use R libraries
  #   Some kind of test to see if temp_digest.bin was formed 

  openssl_cmd <- "openssl"
  digest_args <- paste("dgst -sha256 -sign", 
                       manta_globals$ssl_key_path, 
                       "-out temp_digest.bin", 
                       sep=" ")
  encrypt_args <- "enc -in temp_digest.bin -e -a"

  the_time_now <- format(as.POSIXlt(Sys.time(), "UTC"), 
                         "%a, %d %h %Y %H:%M:%S GMT")
  sig_to_digest <- paste("date:",the_time_now, sep=" ")

  # write sig_to_digest to a binary file without any CR
  con <- file("sig_to_digest.bin", 'wb')
  writeBin(charToRaw(sig_to_digest), con)
  close(con)

  # digest and encrypt
  system2(openssl_cmd, 
          args=digest_args, 
          stdin="sig_to_digest.bin", 
          stdout=FALSE)
  sig_encrypted <- paste(system2(openssl_cmd, 
                                 args=encrypt_args, 
                                 stdout=TRUE), 
                         collapse='')
  gone <- file.remove("temp_digest.bin")
  gone <- file.remove("sig_to_digest.bin")

  signed <- list(time_now=the_time_now, signature=sig_encrypted)
  return(signed)
}
