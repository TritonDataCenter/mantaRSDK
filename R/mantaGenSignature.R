# TODO: 
#   Windows openssl / or use R libraries
#   Some kind of test to see if temp_digest.bin was formed 
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
  if (file.exists("sig_to_digest.bin")) file.remove("sig_to_digest.bin")
  if (file.exists("temp_digest.bin")) file.remove("temp_digest.bin")
  

  openssl_cmd <- "openssl"
  digest_args <- paste("dgst -sha256 -sign", 
                       manta_globals$ssl_key_path, 
                       "-out temp_digest.bin", 
                       sep=" ")
  encrypt_args <- "enc -in temp_digest.bin -e -a"

  the_time_now <- format(as.POSIXlt(Sys.time(), "UTC"), 
                         "%a, %d %b %Y %H:%M:%S GMT")
  sig_to_digest <- paste("date:",the_time_now, sep=" ")

  # write sig_to_digest to a binary file without any CR
  con <- file("sig_to_digest.bin", 'wb')
  writeBin(charToRaw(sig_to_digest), con)
  flush(con)
  close(con)

  # digest and encrypt
  system2(openssl_cmd, 
          args=digest_args, 
          stdin="sig_to_digest.bin",
          wait = TRUE, 
          stdout=FALSE)
  sig_encrypted <- paste(system2(openssl_cmd, 
                                 args=encrypt_args,
                                 wait = TRUE, 
                                 stdout=TRUE), 
                         collapse='')
  
  gone <- file.remove("sig_to_digest.bin")
  gone <- file.remove("temp_digest.bin")
  

  signed <- list(time_now=the_time_now, signature=sig_encrypted)
  return(signed)
}
