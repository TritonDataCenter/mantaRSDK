# TODO: 
#   Windows openssl / or use R libraries
# Roxygen Comments mantaGenSignature
#' Get time, digest and encrypt for HTTPS authentication
#'
#'
#' Not exported.
#' Gets system time and converts to web time format.
#' Implements HTTPS Signature according to Mark Cavage Draft
#' \code{http://tools.ietf.org/html/draft-cavage-http-signatures-00}
#' using \code{system2} calls to \code{openSSL} binary.
#' Under certain conditions, Windows \code{system2} will still have
#' a lock on "temp_digest.bin", despite using \code{wait = TRUE}
#' generating a permissions Warning on the \code{file.remove} command.
#' This function is slated for replacement.
#'
#' @return signed time and rsa-sha256 signature
#'
#' @keywords Manta, manta
#'
mantaGenSignature <-
function() {
  pid <- as.character(Sys.getpid())
  sigfile <- paste("sig_to_digest_", pid, ".bin", sep="")
  digfile <- paste("temp_digest_", pid, ".bin", sep="")

  if (file.exists(sigfile)) file.remove(sigfile)
  if (file.exists(digfile)) file.remove(digfile)
  

  openssl_cmd <- "openssl"
  digest_args <- paste("dgst -sha256 -sign", 
                       manta_globals$ssl_key_path, 
                       "-out ",
                       digfile, 
                       sep=" ")
  encrypt_args <- paste("enc -in ", digfile , " -e -a", sep="")

  the_time_now <- format(as.POSIXlt(Sys.time(), "UTC"), 
                         "%a, %d %b %Y %H:%M:%S GMT")
  sig_to_digest <- paste("date:",the_time_now, sep=" ")

  # write sig_to_digest to a binary file without any CR
  con <- file(sigfile, 'wb')
  writeBin(charToRaw(sig_to_digest), con)
  flush(con)
  close(con)

  # digest and encrypt
  system2(openssl_cmd, 
          args = digest_args, 
          stdin = sigfile,
          wait = TRUE, 
          stdout=FALSE)
  sig_encrypted <- paste(system2(openssl_cmd, 
                                 args=encrypt_args,
                                 wait = TRUE, 
                                 stdout=TRUE), 
                         collapse='')
  
  gone <- file.remove(sigfile)
  gone <- file.remove(digfile)
  

  signed <- list(time_now=the_time_now, signature=sig_encrypted)
  return(signed)
}
